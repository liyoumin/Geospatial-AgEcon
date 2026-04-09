#include <WiFi.h>
#include <HTTPClient.h>
#include <NetworkClientSecure.h>

const char* ssid = "iPhone Youmin li";
const char* password = "123456789";
String apiKey = "YOUR_NEW_API_KEY";

void setup() {
  Serial.begin(115200);
  delay(1000);

  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi...");
  }

  Serial.println("Connected to WiFi");
  Serial.print("IP: ");
  Serial.println(WiFi.localIP());

  delay(3000);  // let hotspot internet settle

  NetworkClientSecure client;
  client.setInsecure();   // quick test only

  HTTPClient http;
  http.setConnectTimeout(15000);
  http.setTimeout(15000);

  String url = "https://api.openweathermap.org/data/2.5/weather?q=Tampa,FL,US&appid=" + apiKey + "&units=metric";
  Serial.println(url);

  if (http.begin(client, url)) {
    int httpCode = http.GET();

    Serial.print("HTTP code: ");
    Serial.println(httpCode);
    Serial.print("HTTP text: ");
    Serial.println(HTTPClient::errorToString(httpCode));

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
    }
    http.end();
  } else {
    Serial.println("http.begin failed");
  }
}

void loop() {}
