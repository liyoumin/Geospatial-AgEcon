/*
  ESP32 Dev Module code for Smart Irrigation System
  -------------------------------------------------
  Matches Mega2560 protocol. 192.168.4.1

  Functions:
  - Reads telemetry from Mega2560 over UART
  - Hosts a local web dashboard on Wi-Fi
  - Sends commands back to Mega:
      AUTO
      MAN
      START
      STOP
      VOL=xxx

  ESP32 board: "ESP32 Dev Module"
*/

#include <WiFi.h>
#include <WebServer.h>

// ===================== WIFI =====================
// Option 1: fill in your Wi-Fi credentials
const char* WIFI_SSID = "";
const char* WIFI_PASS = "";

// Option 2: if WIFI_SSID is blank, ESP32 starts its own AP
const char* AP_SSID   = "SmartIrrigationESP32";
const char* AP_PASS   = "12345678";

// ===================== UART TO MEGA =====================
// ESP32 Serial2 pins (change if your wiring is different)
static const int MEGA_RX_PIN = 16;   // ESP32 RX2  <- Mega TX1 (through voltage divider)
static const int MEGA_TX_PIN = 17;   // ESP32 TX2  -> Mega RX1
static const uint32_t MEGA_BAUD = 9600;

HardwareSerial MegaSerial(2);
WebServer server(80);

// ===================== TELEMETRY STATE =====================
struct Telemetry {
  String mode = "NA";
  int soil_raw = 0;
  float soil_pct = NAN;
  int wl_raw = 0;
  float wl_pct = NAN;
  float temp_c = NAN;
  float humidity = NAN;
  String pump = "OFF";
  unsigned int vol_ml = 100;
  int manual = 0;
  unsigned long lastUpdateMs = 0;
} tel;

String rxBuffer;

// ===================== HELPERS =====================
bool telemetryFresh() {
  return (millis() - tel.lastUpdateMs) < 15000UL;
}

String fmtFloat(float x, int digits = 1) {
  if (isnan(x)) return "--";
  return String(x, digits);
}

String jsonFloat(float x, int digits = 1) {
  if (isnan(x)) return "null";
  return String(x, digits);
}

String currentIP() {
  wifi_mode_t mode = WiFi.getMode();
  if (mode == WIFI_AP || mode == WIFI_AP_STA) {
    return WiFi.softAPIP().toString();
  }
  return WiFi.localIP().toString();
}

void sendMegaCommand(String cmd) {
  cmd.trim();
  if (cmd.length() == 0) return;
  MegaSerial.println(cmd);

  Serial.print("TX -> MEGA: ");
  Serial.println(cmd);
}

void updateField(const String& key, const String& val) {
  if (key == "mode") {
    tel.mode = val;
  } else if (key == "soil_raw") {
    tel.soil_raw = val.toInt();
  } else if (key == "soil_pct") {
    tel.soil_pct = val.toFloat();
  } else if (key == "wl_raw") {
    tel.wl_raw = val.toInt();
  } else if (key == "wl_pct") {
    tel.wl_pct = val.toFloat();
  } else if (key == "temp_c") {
    tel.temp_c = val.toFloat();
  } else if (key == "humidity") {
    tel.humidity = val.toFloat();
  } else if (key == "pump") {
    tel.pump = val;
  } else if (key == "vol_ml") {
    tel.vol_ml = (unsigned int)val.toInt();
  } else if (key == "manual") {
    tel.manual = val.toInt();
  }
}

void parseTelemetryLine(String line) {
  line.trim();
  if (line.length() == 0) return;

  int start = 0;
  while (start < line.length()) {
    int comma = line.indexOf(',', start);
    String token;

    if (comma == -1) {
      token = line.substring(start);
      start = line.length();
    } else {
      token = line.substring(start, comma);
      start = comma + 1;
    }

    token.trim();
    int eq = token.indexOf('=');
    if (eq == -1) continue;

    String key = token.substring(0, eq);
    String val = token.substring(eq + 1);
    key.trim();
    val.trim();

    updateField(key, val);
  }

  tel.lastUpdateMs = millis();

  Serial.print("RX <- MEGA: ");
  Serial.println(line);
}

void readMegaSerial() {
  while (MegaSerial.available()) {
    char c = (char)MegaSerial.read();

    if (c == '\r') continue;

    if (c == '\n') {
      if (rxBuffer.length() > 0) {
        parseTelemetryLine(rxBuffer);
        rxBuffer = "";
      }
    } else {
      if (rxBuffer.length() < 300) {
        rxBuffer += c;
      } else {
        rxBuffer = "";
      }
    }
  }
}

// ===================== WIFI =====================
void startAccessPoint() {
  WiFi.mode(WIFI_AP);
  WiFi.softAP(AP_SSID, AP_PASS);

  Serial.println();
  Serial.println("ESP32 started in AP mode.");
  Serial.print("AP SSID: ");
  Serial.println(AP_SSID);
  Serial.print("AP IP: ");
  Serial.println(WiFi.softAPIP());
}

void connectWiFi() {
  if (strlen(WIFI_SSID) == 0) {
    startAccessPoint();
    return;
  }

  WiFi.mode(WIFI_STA);
  WiFi.begin(WIFI_SSID, WIFI_PASS);

  Serial.print("Connecting to Wi-Fi");
  unsigned long t0 = millis();

  while (WiFi.status() != WL_CONNECTED && millis() - t0 < 15000UL) {
    delay(500);
    Serial.print(".");
  }
  Serial.println();

  if (WiFi.status() == WL_CONNECTED) {
    Serial.println("Wi-Fi connected.");
    Serial.print("ESP32 IP: ");
    Serial.println(WiFi.localIP());
  } else {
    Serial.println("Wi-Fi failed. Falling back to AP mode.");
    WiFi.disconnect(true, true);
    delay(500);
    startAccessPoint();
  }
}

// ===================== WEB =====================
String makeJson() {
  String s = "{";
  s += "\"online\":";
  s += (telemetryFresh() ? "true" : "false");
  s += ",\"mode\":\"" + tel.mode + "\"";
  s += ",\"soil_raw\":" + String(tel.soil_raw);
  s += ",\"soil_pct\":" + jsonFloat(tel.soil_pct, 1);
  s += ",\"wl_raw\":" + String(tel.wl_raw);
  s += ",\"wl_pct\":" + jsonFloat(tel.wl_pct, 1);
  s += ",\"temp_c\":" + jsonFloat(tel.temp_c, 1);
  s += ",\"humidity\":" + jsonFloat(tel.humidity, 1);
  s += ",\"pump\":\"" + tel.pump + "\"";
  s += ",\"vol_ml\":" + String(tel.vol_ml);
  s += ",\"manual\":" + String(tel.manual);
  s += "}";
  return s;
}

void handleJson() {
  server.send(200, "application/json", makeJson());
}

void handleCmd() {
  if (!server.hasArg("x")) {
    server.send(400, "text/plain", "Missing x");
    return;
  }

  String cmd = server.arg("x");
  cmd.trim();
  cmd.toUpperCase();

  bool valid = false;

  if (cmd == "AUTO" || cmd == "MAN" || cmd == "START" || cmd == "STOP") {
    valid = true;
  } else if (cmd.startsWith("VOL=")) {
    String n = cmd.substring(4);
    n.trim();
    int v = n.toInt();
    if (v > 0) valid = true;
  }

  if (!valid) {
    server.send(400, "text/plain", "Invalid command");
    return;
  }

  sendMegaCommand(cmd);
  server.send(200, "text/plain", "OK");
}

void handleRoot() {
  String html = R"rawliteral(
<!DOCTYPE html>
<html>
<head>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Smart Irrigation ESP32</title>
  <style>
    body {
      font-family: Arial, sans-serif;
      margin: 20px;
      background: #f4f7fb;
      color: #1f2937;
    }
    .card {
      background: white;
      padding: 16px;
      border-radius: 14px;
      box-shadow: 0 2px 10px rgba(0,0,0,.08);
      margin-bottom: 16px;
    }
    .grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
      gap: 12px;
    }
    .item {
      background: #f9fafb;
      border-radius: 10px;
      padding: 10px;
    }
    .label {
      font-size: 12px;
      color: #6b7280;
      margin-bottom: 4px;
    }
    .value {
      font-size: 20px;
      font-weight: 700;
    }
    button {
      padding: 12px 16px;
      border: none;
      border-radius: 10px;
      margin: 6px 6px 6px 0;
      font-size: 15px;
      cursor: pointer;
    }
    input[type=number] {
      padding: 10px;
      border-radius: 8px;
      border: 1px solid #cbd5e1;
      width: 130px;
      font-size: 16px;
    }
    .small {
      color: #6b7280;
      font-size: 13px;
    }
  </style>
</head>
<body>
  <div class="card">
    <h2>Smart Irrigation Dashboard</h2>
    <div class="small">ESP32 IP: <b>)rawliteral";
  html += currentIP();
  html += R"rawliteral(</b></div>
    <div class="small">Refresh every 2 seconds</div>
  </div>

  <div class="card">
    <div class="grid">
      <div class="item"><div class="label">Link</div><div class="value" id="online">--</div></div>
      <div class="item"><div class="label">Mode</div><div class="value" id="mode">--</div></div>
      <div class="item"><div class="label">Pump</div><div class="value" id="pump">--</div></div>
      <div class="item"><div class="label">Manual</div><div class="value" id="manual">--</div></div>
      <div class="item"><div class="label">Soil %</div><div class="value" id="soil_pct">--</div></div>
      <div class="item"><div class="label">Soil Raw</div><div class="value" id="soil_raw">--</div></div>
      <div class="item"><div class="label">Water %</div><div class="value" id="wl_pct">--</div></div>
      <div class="item"><div class="label">Water Raw</div><div class="value" id="wl_raw">--</div></div>
      <div class="item"><div class="label">Temp (C)</div><div class="value" id="temp_c">--</div></div>
      <div class="item"><div class="label">Humidity (%)</div><div class="value" id="humidity">--</div></div>
      <div class="item"><div class="label">Target Volume (mL)</div><div class="value" id="vol_ml">--</div></div>
    </div>
  </div>

  <div class="card">
    <h3>Commands</h3>
    <button onclick="sendCmd('AUTO')">AUTO</button>
    <button onclick="sendCmd('MAN')">MANUAL</button>
    <button onclick="sendCmd('START')">START</button>
    <button onclick="sendCmd('STOP')">STOP</button>
    <br><br>
    <input id="vol" type="number" min="1" value="100">
    <button onclick="setVolume()">Set Volume</button>
  </div>

  <script>
    function showValue(id, val) {
      document.getElementById(id).innerText = (val === null || val === undefined) ? '--' : val;
    }

    async function refreshData() {
      try {
        const res = await fetch('/json');
        const d = await res.json();

        showValue('online', d.online ? 'ONLINE' : 'OFFLINE');
        showValue('mode', d.mode);
        showValue('pump', d.pump);
        showValue('manual', d.manual === 1 ? 'YES' : 'NO');
        showValue('soil_pct', d.soil_pct);
        showValue('soil_raw', d.soil_raw);
        showValue('wl_pct', d.wl_pct);
        showValue('wl_raw', d.wl_raw);
        showValue('temp_c', d.temp_c);
        showValue('humidity', d.humidity);
        showValue('vol_ml', d.vol_ml);
      } catch (e) {
        showValue('online', 'NO DATA');
      }
    }

    async function sendCmd(cmd) {
      await fetch('/cmd?x=' + encodeURIComponent(cmd));
      setTimeout(refreshData, 250);
    }

    function setVolume() {
      const v = document.getElementById('vol').value.trim();
      if (v.length > 0) {
        sendCmd('VOL=' + v);
      }
    }

    refreshData();
    setInterval(refreshData, 2000);
  </script>
</body>
</html>
)rawliteral";

  server.send(200, "text/html", html);
}

void setupWeb() {
  server.on("/", handleRoot);
  server.on("/json", handleJson);
  server.on("/cmd", handleCmd);
  server.begin();

  Serial.println("Web server started.");
}

// ===================== SETUP / LOOP =====================
void setup() {
  Serial.begin(115200);
  delay(800);

  MegaSerial.begin(MEGA_BAUD, SERIAL_8N1, MEGA_RX_PIN, MEGA_TX_PIN);

  Serial.println();
  Serial.println("ESP32 Smart Irrigation Interface Starting...");

  connectWiFi();
  setupWeb();

  Serial.println("Ready.");
}

void loop() {
  readMegaSerial();
  server.handleClient();
}
