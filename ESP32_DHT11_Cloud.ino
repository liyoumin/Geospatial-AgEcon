#include <WiFi.h>
#include <DHT.h>
#include <ThingSpeak.h>

const char* ssid = "iPhone Youmin li";
const char* password = "12345678";

WiFiClient client;

unsigned long myChannelID = 3347147;
const char * myWriteAPIKey = "V85NE5L7FEY0ZIUU";

#define DHTPIN 23
#define DHTTYPE DHT11
DHT dht(DHTPIN, DHTTYPE);

unsigned long lastTime = 0;
const unsigned long timerDelay = 30000;   // 30 seconds

float temperature = NAN;
float humidity = NAN;

void connectWiFi() {
  WiFi.mode(WIFI_STA);
  WiFi.setSleep(false);

  // optional DNS setting
  IPAddress local_IP(0, 0, 0, 0);
  IPAddress gateway(0, 0, 0, 0);
  IPAddress subnet(0, 0, 0, 0);
  IPAddress primaryDNS(8, 8, 8, 8);
  IPAddress secondaryDNS(1, 1, 1, 1);
  WiFi.config(local_IP, gateway, subnet, primaryDNS, secondaryDNS);

  Serial.println("Connecting to WiFi...");
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println();
  Serial.println("Connected to WiFi");
  Serial.print("IP: ");
  Serial.println(WiFi.localIP());

  IPAddress tsIP;
  if (WiFi.hostByName("api.thingspeak.com", tsIP)) {
    Serial.print("ThingSpeak IP: ");
    Serial.println(tsIP);
  } else {
    Serial.println("DNS failed for api.thingspeak.com");
  }
}

bool readDHT() {
  humidity = dht.readHumidity();
  temperature = dht.readTemperature();

  if (isnan(humidity) || isnan(temperature)) {
    Serial.println("Failed to read from DHT sensor.");
    return false;
  }

  Serial.print("Temperature (C): ");
  Serial.println(temperature);
  Serial.print("Humidity (%): ");
  Serial.println(humidity);

  return true;
}

void setup() {
  Serial.begin(115200);
  delay(1000);

  dht.begin();
  delay(2000);   // let DHT stabilize

  connectWiFi();
  ThingSpeak.begin(client);

  lastTime = millis() - timerDelay;   // send first update soon
}

void loop() {
  if (WiFi.status() != WL_CONNECTED) {
    Serial.println("WiFi lost. Reconnecting...");
    connectWiFi();
  }

  if ((millis() - lastTime) >= timerDelay) {
    Serial.println("Reading sensor and sending to ThingSpeak...");

    if (readDHT()) {
      ThingSpeak.setField(1, temperature);  // temperature
      ThingSpeak.setField(7, humidity);     // humidity

      // future use:
      // ThingSpeak.setField(2, soilMoisture);
      // ThingSpeak.setField(3, waterLevel);
      // ThingSpeak.setField(4, weatherData);
      // ThingSpeak.setField(5, irrigationVolume);
      // ThingSpeak.setField(6, irrigationStatus);

      int x = ThingSpeak.writeFields(myChannelID, myWriteAPIKey);

      if (x == 200) {
        Serial.println("Channel update successful.");
      } else {
        Serial.print("ThingSpeak write failed, code: ");
        Serial.println(x);
      }
    }

    lastTime = millis();
  }
}
