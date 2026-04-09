#include <DHT.h>
#include <LiquidCrystal.h>
#include <Keypad.h>

// ===================== PINS =====================
const int PIN_SOIL   = A0;
const int PIN_DHT    = 2;
const int PIN_RELAY  = 8;
const int PIN_MODE_BTN = 3;   // push button to toggle AUTO/MANUAL

// LCD1602 4-bit: RS, E, D4, D5, D6, D7
LiquidCrystal lcd(22, 23, 24, 25, 26, 27);

// ===================== DHT ======================
#define DHTTYPE DHT11
DHT dht(PIN_DHT, DHTTYPE);

// ===================== KEYPAD ===================
const byte ROWS = 4;
const byte COLS = 4;

char keys[ROWS][COLS] = {
  {'1','2','3','A'},
  {'4','5','6','B'},
  {'7','8','9','C'},
  {'*','0','#','D'}
};

byte rowPins[ROWS] = {30, 31, 32, 33};
byte colPins[COLS] = {34, 35, 36, 37};

Keypad keypad = Keypad(makeKeymap(keys), rowPins, colPins, ROWS, COLS);

// ===================== TIMING ===================
unsigned long lastReadMs      = 0;
unsigned long lastSendMs      = 0;
unsigned long lastLcdFlipMs   = 0;
unsigned long modeBtnLastMs   = 0;
unsigned long manualStartMs   = 0;
unsigned long pumpStartMs     = 0;
unsigned long lastPumpOffMs   = 0;

const unsigned long READ_INTERVAL_MS = 3000;
const unsigned long SEND_INTERVAL_MS = 5000;
const unsigned long LCD_FLIP_MS      = 2000;
const unsigned long DEBOUNCE_MS      = 250;
const unsigned long MAX_ON_MS        = 10UL * 60UL * 1000UL; // 10 min
const unsigned long MIN_OFF_MS       = 10UL * 1000UL;        // 10 sec protect relay

// ===================== CALIBRATION ==============
int soilDryADC = 850;   // update after calibration
int soilWetADC = 350;   // update after calibration

float baseLOW  = 30.0;  // auto ON below this
float baseHIGH = 40.0;  // auto OFF above this

float hotC  = 30.0;
float dryRH = 30.0;
float bias  = 3.0;

// Pump flow estimate for manual volume control
// Example: if pump fills 250 mL in 10 seconds, then flow = 25 mL/s
const float PUMP_FLOW_ML_PER_SEC = 25.0;

// Relay polarity
const bool RELAY_ACTIVE_LOW = false;

// ===================== STATE ====================
bool pumpOn = false;
bool autoMode = true;
bool lcdPage1 = true;
bool manualRunning = false;

float soilPct = NAN;
float T = NAN;
float RH = NAN;
float LOW_T = NAN;
float HIGH_T = NAN;
int soilADC = 0;

// manual target irrigation volume
unsigned int targetVolumeML = 100;   // default
String keypadBuffer = "";
unsigned long manualRunDurationMs = 0;

// ===================== HELPERS ==================
void relayWrite(bool on) {
  if (!RELAY_ACTIVE_LOW) digitalWrite(PIN_RELAY, on ? HIGH : LOW);
  else                   digitalWrite(PIN_RELAY, on ? LOW : HIGH);
}

float soilAdcToPercent(int adc) {
  int lo = min(soilWetADC, soilDryADC);
  int hi = max(soilWetADC, soilDryADC);
  adc = constrain(adc, lo, hi);

  float pct = 100.0 * (float)(soilDryADC - adc) / (float)(soilDryADC - soilWetADC);
  return constrain(pct, 0.0, 100.0);
}

void computeThresholds(float tC, float rh, float &lowT, float &highT) {
  lowT = baseLOW;
  highT = baseHIGH;

  if (!isnan(tC) && !isnan(rh) && tC >= hotC && rh <= dryRH) {
    lowT  += bias;
    highT += bias;
  }
}

void setPump(bool on) {
  if (on == pumpOn) return;

  if (on) {
    if (millis() - lastPumpOffMs < MIN_OFF_MS) return;
    pumpOn = true;
    pumpStartMs = millis();
    relayWrite(true);
  } else {
    pumpOn = false;
    manualRunning = false;
    lastPumpOffMs = millis();
    relayWrite(false);
  }
}

void readSensors() {
  soilADC = analogRead(PIN_SOIL);
  soilPct = soilAdcToPercent(soilADC);

  float newRH = dht.readHumidity();
  float newT  = dht.readTemperature();

  // keep last good DHT values
  if (!isnan(newRH) && !isnan(newT)) {
    RH = newRH;
    T = newT;
  }

  computeThresholds(T, RH, LOW_T, HIGH_T);
}

void autoControlLogic() {
  if (pumpOn && (millis() - pumpStartMs > MAX_ON_MS)) {
    setPump(false);
    return;
  }

  if (isnan(soilPct)) {
    setPump(false);
    return;
  }

  if (!pumpOn && soilPct < LOW_T) setPump(true);
  else if (pumpOn && soilPct > HIGH_T) setPump(false);
}

void startManualIrrigation(unsigned int volML) {
  if (volML == 0) return;

  manualRunDurationMs = (unsigned long)((volML / PUMP_FLOW_ML_PER_SEC) * 1000.0);
  manualStartMs = millis();
  manualRunning = true;
  setPump(true);
}

void handleManualIrrigation() {
  if (manualRunning) {
    if (millis() - manualStartMs >= manualRunDurationMs) {
      setPump(false);
    }
  }
}

void toggleMode() {
  autoMode = !autoMode;
  manualRunning = false;
  setPump(false);
}

void handleModeButton() {
  if (digitalRead(PIN_MODE_BTN) == LOW) {
    if (millis() - modeBtnLastMs > DEBOUNCE_MS) {
      modeBtnLastMs = millis();
      toggleMode();
    }
  }
}

void handleKeypad() {
  char key = keypad.getKey();
  if (!key) return;

  Serial.print("Key=");
  Serial.println(key);

  if (key >= '0' && key <= '9') {
    if (keypadBuffer.length() < 4) keypadBuffer += key;
    return;
  }

  switch (key) {
    case '*':   // clear entry
      keypadBuffer = "";
      break;

    case '#':   // save entered volume
      if (keypadBuffer.length() > 0) {
        targetVolumeML = keypadBuffer.toInt();
        keypadBuffer = "";
      }
      break;

    case 'A':   // start manual irrigation
      if (!autoMode) {
        startManualIrrigation(targetVolumeML);
      }
      break;

    case 'B':   // stop pump
      setPump(false);
      break;

    case 'D':   // force AUTO mode
      autoMode = true;
      manualRunning = false;
      setPump(false);
      break;

    case 'C':   // force MANUAL mode
      autoMode = false;
      setPump(false);
      break;
  }
}

void lcdStartup() {
  lcd.clear();
  lcd.setCursor(0,0);
  lcd.print("Smart Irrig.");
  lcd.setCursor(0,1);
  lcd.print("Mega2560 Ready");
  delay(1500);
  lcd.clear();
}

void lcdPageSoil() {
  lcd.setCursor(0,0);
  lcd.print(autoMode ? "AUTO " : "MAN  ");
  lcd.print("S:");
  lcd.print((int)soilPct);
  lcd.print("%");
  lcd.print(pumpOn ? " ON " : " OFF");
  lcd.print("  ");

  lcd.setCursor(0,1);
  lcd.print("T:");
  lcd.print((int)(isnan(T) ? 0 : T));
  lcd.print("C ");
  lcd.print((int)(isnan(RH) ? 0 : RH));
  lcd.print("% ");
  lcd.print("v:");
  lcd.print(targetVolumeML);
  lcd.print("m ");
}

void updateLCD() {
  lcdPageSoil();
}
void telemetry() {
  Serial.print("ADC=");   Serial.print(soilADC);
  Serial.print(",Soil="); Serial.print(soilPct,1);
  Serial.print(",T=");    Serial.print(T,1);
  Serial.print(",RH=");   Serial.print(RH,1);
  Serial.print(",Pump="); Serial.print(pumpOn ? 1 : 0);
  Serial.print(",Mode="); Serial.print(autoMode ? "AUTO" : "MAN");
  Serial.print(",LOW=");  Serial.print(LOW_T,1);
  Serial.print(",HIGH="); Serial.print(HIGH_T,1);
  Serial.print(",Vol=");  Serial.print(targetVolumeML);
  Serial.print(",RunMS=");Serial.println(manualRunDurationMs);
}

// ===================== SETUP ====================
void setup() {
  Serial.begin(9600);

  pinMode(PIN_RELAY, OUTPUT);
  relayWrite(false);

  pinMode(PIN_MODE_BTN, INPUT_PULLUP);

  dht.begin();
  delay(1000);

  lcd.begin(16, 2);
  lcdStartup();

  readSensors();
  updateLCD();
}

// ===================== LOOP =====================
void loop() {
  unsigned long now = millis();

  handleModeButton();
  handleKeypad();

  if (now - lastReadMs >= READ_INTERVAL_MS) {
    lastReadMs = now;
    readSensors();

    if (autoMode) {
      autoControlLogic();
    } else {
      handleManualIrrigation();
    }
  }

  if (!autoMode) {
    handleManualIrrigation();
  }

  updateLCD();

  if (now - lastSendMs >= SEND_INTERVAL_MS) {
    lastSendMs = now;
    telemetry();
  }
}