#include <WiFi.h>
#include <WebServer.h>
#include <HTTPClient.h>

// =====================================================
// HOME WIFI: ESP32 uses this for OpenWeather + ThingSpeak
// =====================================================
const char* HOME_WIFI_SSID = "YOUR_HOME_WIFI_NAME";
const char* HOME_WIFI_PASS = "YOUR_HOME_WIFI_PASSWORD";

// =====================================================
// ESP32 OWN WIFI: phone connects to this
// Phone browser: http://192.168.4.1
// =====================================================
const char* AP_SSID = "SmartIrrigation_Youmin";
const char* AP_PASS = "12345678";

IPAddress AP_IP(192, 168, 4, 1);
IPAddress AP_GATEWAY(192, 168, 4, 1);
IPAddress AP_SUBNET(255, 255, 255, 0);

// =====================================================
// OpenWeather: Woodland, CA
// =====================================================
String apiKey = "1da5602c98819cc5e77582d0676746f8";

const char* WEATHER_LAT = "38.6785";
const char* WEATHER_LON = "-121.7733";

// =====================================================
// ThingSpeak
// =====================================================
const char* THINGSPEAK_WRITE_API_KEY = "V85NE5L7FEY0ZIUU";

const unsigned long THINGSPEAK_INTERVAL_MS = 20000;
const unsigned long WEATHER_INTERVAL_MS = 60000;
const unsigned long WIFI_RECONNECT_INTERVAL_MS = 15000;

// =====================================================
// Mega2560 UART reading Mega Serial.begin(9600), so connect:
// Mega TX0 pin 1 -> ESP32 GPIO16 RX2
// GND -> GND
// =====================================================
#define ESP32_RX_FROM_MEGA 16
#define ESP32_TX_TO_MEGA   17
#define MEGA_BAUD          9600

HardwareSerial MegaSerial(2);
WebServer server(80);

// =====================================================
// ESP32 water level sensor
// Connect water level signal to ESP32 GPIO34.
// =====================================================
const int PIN_WATER_LEVEL = 34;

// Calibrate these after testing Serial Monitor.
// Empty tank raw ADC
int WATER_EMPTY_ADC = 0;

// Full tank raw ADC
int WATER_FULL_ADC = 4095;

// =====================================================
// Live data parsed from your current Mega code
// Mega line example:
// ADC=630,Soil=44.0,T=23.0,RH=45.0,Pump=0,Mode=AUTO,LOW=30.0,HIGH=40.0,Vol=100,RunMS=4000
// =====================================================
String rxBuffer = "";
String lastMegaLine = "";

int soilADC = -1;
float soilPct = NAN;
float dhtTempC = NAN;
float dhtHumPct = NAN;
int pumpStatus = 0;
String modeStatus = "--";
float lowThreshold = NAN;
float highThreshold = NAN;
int irrigationVolumeML = -1;
unsigned long runMS = 0;

unsigned long lastMegaMs = 0;

// =====================================================
// ESP32 water level
// =====================================================
int waterLevelADC = 0;
float waterLevelPct = NAN;

// =====================================================
// OpenWeather data
// =====================================================
float weatherTempC = NAN;      // OpenWeather main.temp
float weatherHumPct = NAN;     // OpenWeather main.humidity
float weatherWindMS = NAN;
String weatherMain = "--";
String weatherDesc = "--";
String weatherCity = "Woodland";
bool weatherOK = false;
unsigned long lastWeatherMs = 0;

// =====================================================
// ThingSpeak status
// =====================================================
unsigned long lastThingSpeakMs = 0;
int lastThingSpeakHttpCode = 0;
String lastThingSpeakResponse = "--";

// =====================================================
// WiFi status
// =====================================================
unsigned long lastWifiReconnectMs = 0;

// =====================================================
// Helper functions
// =====================================================
String getValueFromLine(const String& line, const String& key) {
  String token = key + "=";
  int start = line.indexOf(token);
  if (start < 0) return "";

  start += token.length();
  int end = line.indexOf(',', start);
  if (end < 0) end = line.length();

  String value = line.substring(start, end);
  value.trim();
  return value;
}

String jsonEscape(String s) {
  s.replace("\\", "\\\\");
  s.replace("\"", "\\\"");
  s.replace("\n", " ");
  s.replace("\r", " ");
  return "\"" + s + "\"";
}

String jsonFloat(float v, int digits = 1) {
  if (isnan(v)) return "null";
  return String(v, digits);
}

String jsonInt(int v) {
  if (v < 0) return "null";
  return String(v);
}

String urlEncode(String str) {
  String encoded = "";
  char c;
  char code0;
  char code1;

  for (int i = 0; i < str.length(); i++) {
    c = str.charAt(i);

    if (isalnum(c)) {
      encoded += c;
    } else if (c == ' ') {
      encoded += "%20";
    } else {
      code1 = (c & 0xf) + '0';
      if ((c & 0xf) > 9) code1 = (c & 0xf) - 10 + 'A';

      c = (c >> 4) & 0xf;
      code0 = c + '0';
      if (c > 9) code0 = c - 10 + 'A';

      encoded += '%';
      encoded += code0;
      encoded += code1;
    }
  }

  return encoded;
}

float extractNumber(const String& payload, const String& key, float fallback = NAN) {
  int start = payload.indexOf(key);
  if (start < 0) return fallback;

  start += key.length();

  while (start < payload.length() &&
         (payload[start] == ' ' || payload[start] == ':')) {
    start++;
  }

  int end = start;
  while (end < payload.length() &&
         (isDigit(payload[end]) || payload[end] == '-' || payload[end] == '.')) {
    end++;
  }

  if (end <= start) return fallback;
  return payload.substring(start, end).toFloat();
}

String extractString(const String& payload, const String& key, String fallback = "--") {
  int start = payload.indexOf(key);
  if (start < 0) return fallback;

  start += key.length();
  int end = payload.indexOf("\"", start);
  if (end < 0) return fallback;

  return payload.substring(start, end);
}

float adcToPercent(int adc, int emptyADC, int fullADC) {
  if (fullADC == emptyADC) return NAN;

  float pct;

  if (fullADC > emptyADC) {
    pct = 100.0 * (float)(adc - emptyADC) / (float)(fullADC - emptyADC);
  } else {
    pct = 100.0 * (float)(emptyADC - adc) / (float)(emptyADC - fullADC);
  }

  if (pct < 0) pct = 0;
  if (pct > 100) pct = 100;

  return pct;
}

// =====================================================
// Read ESP32 water level sensor
// =====================================================
void readWaterLevel() {
  waterLevelADC = analogRead(PIN_WATER_LEVEL);
  waterLevelPct = adcToPercent(waterLevelADC, WATER_EMPTY_ADC, WATER_FULL_ADC);
}

// =====================================================
// Parse your unchanged Mega telemetry line
// =====================================================
void parseMegaLine(String line) {
  line.trim();
  if (line.length() == 0) return;

  // Ignore keypad messages like: Key=A
  if (line.indexOf("ADC=") < 0 || line.indexOf("Soil=") < 0) {
    Serial.print("Ignored Mega line: ");
    Serial.println(line);
    return;
  }

  lastMegaLine = line;
  lastMegaMs = millis();

  String v;

  v = getValueFromLine(line, "ADC");
  if (v.length() > 0) soilADC = v.toInt();

  v = getValueFromLine(line, "Soil");
  if (v.length() > 0) soilPct = v.toFloat();

  v = getValueFromLine(line, "T");
  if (v.length() > 0) dhtTempC = v.toFloat();

  v = getValueFromLine(line, "RH");
  if (v.length() > 0) dhtHumPct = v.toFloat();

  v = getValueFromLine(line, "Pump");
  if (v.length() > 0) pumpStatus = v.toInt();

  v = getValueFromLine(line, "Mode");
  if (v.length() > 0) {
    modeStatus = v;
    modeStatus.toUpperCase();
  }

  v = getValueFromLine(line, "LOW");
  if (v.length() > 0) lowThreshold = v.toFloat();

  v = getValueFromLine(line, "HIGH");
  if (v.length() > 0) highThreshold = v.toFloat();

  v = getValueFromLine(line, "Vol");
  if (v.length() > 0) irrigationVolumeML = v.toInt();

  v = getValueFromLine(line, "RunMS");
  if (v.length() > 0) runMS = v.toInt();

  Serial.print("Parsed Mega: ");
  Serial.println(line);
}

void readMegaSerial() {
  while (MegaSerial.available()) {
    char c = MegaSerial.read();

    if (c == '\n' || c == '\r') {
      if (rxBuffer.length() > 0) {
        parseMegaLine(rxBuffer);
        rxBuffer = "";
      }
    } else {
      if (rxBuffer.length() < 220) {
        rxBuffer += c;
      } else {
        rxBuffer = "";
      }
    }
  }
}

// =====================================================
// WiFi setup
// =====================================================
void setupWiFi() {
  WiFi.mode(WIFI_AP_STA);
  WiFi.setSleep(false);

  WiFi.softAPConfig(AP_IP, AP_GATEWAY, AP_SUBNET);
  bool apOK = WiFi.softAP(AP_SSID, AP_PASS);

  Serial.println();
  Serial.println("===== ESP32 AP MODE =====");
  Serial.print("AP started: ");
  Serial.println(apOK ? "YES" : "NO");
  Serial.print("Phone WiFi name: ");
  Serial.println(AP_SSID);
  Serial.print("Phone browser: http://");
  Serial.println(WiFi.softAPIP());

  Serial.println();
  Serial.println("===== ESP32 HOME WIFI STA MODE =====");
  Serial.print("Connecting to home WiFi: ");
  Serial.println(HOME_WIFI_SSID);

  WiFi.begin(HOME_WIFI_SSID, HOME_WIFI_PASS);

  unsigned long startAttempt = millis();
  while (WiFi.status() != WL_CONNECTED && millis() - startAttempt < 12000) {
    delay(300);
    Serial.print(".");
  }

  Serial.println();

  if (WiFi.status() == WL_CONNECTED) {
    Serial.println("Home WiFi connected.");
    Serial.print("STA IP: ");
    Serial.println(WiFi.localIP());
  } else {
    Serial.println("Home WiFi not connected yet. AP still works at 192.168.4.1.");
  }
}

void maintainWiFi() {
  if (WiFi.status() == WL_CONNECTED) return;

  if (millis() - lastWifiReconnectMs >= WIFI_RECONNECT_INTERVAL_MS) {
    lastWifiReconnectMs = millis();

    Serial.println("Reconnecting to home WiFi...");
    WiFi.disconnect();
    delay(100);
    WiFi.begin(HOME_WIFI_SSID, HOME_WIFI_PASS);
  }
}

// =====================================================
// OpenWeather update
// =====================================================
void updateWeather(bool force = false) {
  if (!force && millis() - lastWeatherMs < WEATHER_INTERVAL_MS) return;
  lastWeatherMs = millis();

  if (WiFi.status() != WL_CONNECTED) {
    weatherOK = false;
    Serial.println("Weather skipped: home WiFi not connected.");
    return;
  }

  String url = "http://api.openweathermap.org/data/2.5/weather?lat=";
  url += WEATHER_LAT;
  url += "&lon=";
  url += WEATHER_LON;
  url += "&appid=";
  url += apiKey;
  url += "&units=metric";

  HTTPClient http;
  http.setTimeout(6000);
  http.begin(url);

  int httpCode = http.GET();

  if (httpCode == 200) {
    String payload = http.getString();

    weatherTempC = extractNumber(payload, "\"temp\":");
    weatherHumPct = extractNumber(payload, "\"humidity\":");
    weatherWindMS = extractNumber(payload, "\"speed\":");
    weatherMain = extractString(payload, "\"main\":\"", "--");
    weatherDesc = extractString(payload, "\"description\":\"", "--");
    weatherCity = extractString(payload, "\"name\":\"", "Woodland");

    weatherOK = true;

    Serial.println("OpenWeather updated.");
    Serial.print("OpenWeather main.temp C: ");
    Serial.println(weatherTempC);
    Serial.print("OpenWeather main.humidity %: ");
    Serial.println(weatherHumPct);
    Serial.print("OpenWeather main: ");
    Serial.println(weatherMain);
    Serial.print("OpenWeather description: ");
    Serial.println(weatherDesc);
  } else {
    weatherOK = false;
    Serial.print("OpenWeather failed. HTTP code: ");
    Serial.println(httpCode);
  }

  http.end();
}

// =====================================================
// ThingSpeak upload
// =====================================================
void uploadThingSpeak() {
  if (millis() - lastThingSpeakMs < THINGSPEAK_INTERVAL_MS) return;
  lastThingSpeakMs = millis();

  if (WiFi.status() != WL_CONNECTED) {
    Serial.println("ThingSpeak skipped: home WiFi not connected.");
    return;
  }

  String url = "http://api.thingspeak.com/update?api_key=";
  url += THINGSPEAK_WRITE_API_KEY;

  // Your required ThingSpeak field mapping:
  // field1 = DHT11 temperature
  // field2 = soil moisture
  // field3 = water level
  // field4 = OpenWeather main.temp
  // field5 = irrigation volume
  // field6 = irrigation status, pump ON=1, OFF=0
  // field7 = DHT11 humidity
  // field8 = OpenWeather main.humidity

  if (!isnan(dhtTempC))          url += "&field1=" + String(dhtTempC, 1);
  if (!isnan(soilPct))           url += "&field2=" + String(soilPct, 1);
  if (!isnan(waterLevelPct))     url += "&field3=" + String(waterLevelPct, 1);
  if (!isnan(weatherTempC))      url += "&field4=" + String(weatherTempC, 1);
  if (irrigationVolumeML >= 0)   url += "&field5=" + String(irrigationVolumeML);

  url += "&field6=" + String(pumpStatus);

  if (!isnan(dhtHumPct))         url += "&field7=" + String(dhtHumPct, 1);
  if (!isnan(weatherHumPct))     url += "&field8=" + String(weatherHumPct, 1);

  String statusText = "MODE=" + modeStatus +
                      ",PUMP=" + String(pumpStatus) +
                      ",WEATHER_MAIN=" + weatherMain +
                      ",WEATHER_DESC=" + weatherDesc +
                      ",CITY=" + weatherCity +
                      ",WL_ADC=" + String(waterLevelADC);

  url += "&status=" + urlEncode(statusText);

  HTTPClient http;
  http.setTimeout(6000);
  http.begin(url);

  int httpCode = http.GET();
  String response = http.getString();

  lastThingSpeakHttpCode = httpCode;
  lastThingSpeakResponse = response;

  Serial.print("ThingSpeak HTTP code: ");
  Serial.println(httpCode);
  Serial.print("ThingSpeak response: ");
  Serial.println(response);

  if (httpCode == 200 && response == "0") {
    Serial.println("ThingSpeak rejected update. Usually interval too fast or Write API key issue.");
  }

  http.end();
}

// =====================================================
// Web page at 192.168.4.1
// =====================================================
const char MAIN_PAGE[] PROGMEM = R"rawliteral(
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Smart Irrigation ESP32</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    body {
      font-family: Arial, sans-serif;
      background: #f4f7f4;
      margin: 0;
      padding: 16px;
      color: #1f2933;
    }
    h1 {
      font-size: 24px;
      margin-bottom: 4px;
    }
    .sub {
      color: #52616b;
      margin-bottom: 16px;
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(145px, 1fr));
      gap: 12px;
    }
    .card {
      background: white;
      border-radius: 14px;
      padding: 14px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.08);
    }
    .label {
      color: #657786;
      font-size: 13px;
      margin-bottom: 8px;
    }
    .value {
      font-size: 24px;
      font-weight: bold;
    }
    .small {
      font-size: 13px;
      color: #52616b;
      margin-top: 6px;
    }
    .good {
      color: #12805c;
    }
    .bad {
      color: #b42318;
    }
    .footer {
      margin-top: 18px;
      font-size: 12px;
      color: #657786;
      word-break: break-word;
    }
  </style>
</head>
<body>
  <h1>Smart Irrigation Live Dashboard</h1>
  <div class="sub">ESP32 local page: 192.168.4.1</div>

  <div class="grid">
    <div class="card">
      <div class="label">Field 1: DHT11 Temperature</div>
      <div class="value" id="dhtTemp">--</div>
    </div>

    <div class="card">
      <div class="label">Field 2: Soil Moisture</div>
      <div class="value" id="soil">--</div>
    </div>

    <div class="card">
      <div class="label">Field 3: Water Level</div>
      <div class="value" id="waterLevel">--</div>
      <div class="small" id="waterADC">ADC: --</div>
    </div>

    <div class="card">
      <div class="label">Field 4: OpenWeather Temp</div>
      <div class="value" id="weatherTemp">--</div>
      <div class="small">Woodland, CA main.temp</div>
    </div>

    <div class="card">
      <div class="label">Field 5: Irrigation Volume</div>
      <div class="value" id="volume">--</div>
    </div>

    <div class="card">
      <div class="label">Field 6: Irrigation Status</div>
      <div class="value" id="pump">--</div>
      <div class="small">1 = ON, 0 = OFF</div>
    </div>

    <div class="card">
      <div class="label">Field 7: DHT11 Humidity</div>
      <div class="value" id="dhtHum">--</div>
    </div>

    <div class="card">
      <div class="label">Field 8: OpenWeather Humidity</div>
      <div class="value" id="weatherHum">--</div>
    </div>

    <div class="card">
      <div class="label">Weather Main</div>
      <div class="value" id="weatherMain">--</div>
      <div class="small" id="weatherDesc">--</div>
    </div>

    <div class="card">
      <div class="label">Mode</div>
      <div class="value" id="mode">--</div>
    </div>

    <div class="card">
      <div class="label">Home WiFi</div>
      <div class="value" id="wifi">--</div>
      <div class="small" id="staIP">--</div>
    </div>

    <div class="card">
      <div class="label">ThingSpeak</div>
      <div class="value" id="ts">--</div>
      <div class="small" id="tsResp">--</div>
    </div>
  </div>

  <div class="footer">
    Last Mega line: <span id="line">--</span><br>
    Mega data age: <span id="megaAge">--</span><br>
    Weather age: <span id="weatherAge">--</span>
  </div>

<script>
function fmt(v, unit) {
  if (v === null || v === undefined || v === "") return "--";
  return v + unit;
}

async function loadData() {
  try {
    const res = await fetch("/data?x=" + Date.now());
    const d = await res.json();

    document.getElementById("dhtTemp").textContent = fmt(d.dhtTemp, "°C");
    document.getElementById("soil").textContent = fmt(d.soil, "%");
    document.getElementById("waterLevel").textContent = fmt(d.waterLevel, "%");
    document.getElementById("waterADC").textContent = "ADC: " + d.waterADC;

    document.getElementById("weatherTemp").textContent = fmt(d.weatherTemp, "°C");
    document.getElementById("volume").textContent = fmt(d.volume, " mL");
    document.getElementById("pump").textContent = d.pump;
    document.getElementById("dhtHum").textContent = fmt(d.dhtHum, "%");
    document.getElementById("weatherHum").textContent = fmt(d.weatherHum, "%");

    document.getElementById("weatherMain").textContent = d.weatherMain;
    document.getElementById("weatherDesc").textContent = d.weatherDesc;

    document.getElementById("mode").textContent = d.mode;

    document.getElementById("wifi").textContent = d.homeWiFi ? "Connected" : "Not connected";
    document.getElementById("wifi").className = d.homeWiFi ? "value good" : "value bad";
    document.getElementById("staIP").textContent = "STA IP: " + d.staIP;

    document.getElementById("ts").textContent = d.thingspeakCode;
    document.getElementById("tsResp").textContent = "Response: " + d.thingspeakResponse;

    document.getElementById("line").textContent = d.lastMegaLine;
    document.getElementById("megaAge").textContent = fmt(d.megaAgeSec, " sec");
    document.getElementById("weatherAge").textContent = fmt(d.weatherAgeSec, " sec");
  } catch (e) {
    console.log(e);
  }
}

setInterval(loadData, 2000);
loadData();
</script>
</body>
</html>
)rawliteral";

void handleRoot() {
  server.send_P(200, "text/html", MAIN_PAGE);
}

void handleData() {
  String json = "{";

  json += "\"dhtTemp\":" + jsonFloat(dhtTempC, 1) + ",";
  json += "\"soil\":" + jsonFloat(soilPct, 1) + ",";
  json += "\"waterLevel\":" + jsonFloat(waterLevelPct, 1) + ",";
  json += "\"waterADC\":" + String(waterLevelADC) + ",";
  json += "\"weatherTemp\":" + jsonFloat(weatherTempC, 1) + ",";
  json += "\"volume\":" + jsonInt(irrigationVolumeML) + ",";
  json += "\"pump\":" + String(pumpStatus) + ",";
  json += "\"dhtHum\":" + jsonFloat(dhtHumPct, 1) + ",";
  json += "\"weatherHum\":" + jsonFloat(weatherHumPct, 1) + ",";

  json += "\"mode\":" + jsonEscape(modeStatus) + ",";
  json += "\"weatherMain\":" + jsonEscape(weatherMain) + ",";
  json += "\"weatherDesc\":" + jsonEscape(weatherDesc) + ",";
  json += "\"weatherCity\":" + jsonEscape(weatherCity) + ",";
  json += "\"weatherOK\":" + String(weatherOK ? "true" : "false") + ",";

  json += "\"homeWiFi\":" + String(WiFi.status() == WL_CONNECTED ? "true" : "false") + ",";
  json += "\"staIP\":" + jsonEscape(WiFi.localIP().toString()) + ",";
  json += "\"apIP\":" + jsonEscape(WiFi.softAPIP().toString()) + ",";

  json += "\"thingspeakCode\":" + String(lastThingSpeakHttpCode) + ",";
  json += "\"thingspeakResponse\":" + jsonEscape(lastThingSpeakResponse) + ",";

  if (lastMegaMs == 0) {
    json += "\"megaAgeSec\":null,";
  } else {
    json += "\"megaAgeSec\":" + String((millis() - lastMegaMs) / 1000) + ",";
  }

  if (lastWeatherMs == 0) {
    json += "\"weatherAgeSec\":null,";
  } else {
    json += "\"weatherAgeSec\":" + String((millis() - lastWeatherMs) / 1000) + ",";
  }

  json += "\"lastMegaLine\":" + jsonEscape(lastMegaLine);

  json += "}";

  server.send(200, "application/json", json);
}

// =====================================================
// Setup and loop
// =====================================================
void setup() {
  Serial.begin(115200);
  delay(1000);

  analogReadResolution(12);
  analogSetPinAttenuation(PIN_WATER_LEVEL, ADC_11db);

  MegaSerial.begin(MEGA_BAUD, SERIAL_8N1, ESP32_RX_FROM_MEGA, ESP32_TX_TO_MEGA);

  setupWiFi();

  server.on("/", handleRoot);
  server.on("/data", handleData);

  server.on("/weather", []() {
    updateWeather(true);
    handleData();
  });

  server.begin();

  Serial.println();
  Serial.println("Web server started.");
  Serial.println("Connect phone to ESP32 WiFi:");
  Serial.println(AP_SSID);
  Serial.println("Password:");
  Serial.println(AP_PASS);
  Serial.println("Then open browser:");
  Serial.println("http://192.168.4.1");
  Serial.println();

  updateWeather(true);
}

void loop() {
  server.handleClient();

  readMegaSerial();
  readWaterLevel();

  maintainWiFi();

  updateWeather(false);
  uploadThingSpeak();
}
