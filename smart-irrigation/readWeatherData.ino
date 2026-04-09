#include <WiFi.h>
#include <HTTPClient.h>
const char* ssid = "iPhone Youmin li";
const char* password = "12345678";
// Use your OpenWeatherMap API key here
String apiKey = "1da5602c98819cc5e77582d0676746f8";
void setup() {
  // put your setup code here, to run once:
  Serial.begin(115200);
  WiFi.begin(ssid, password);
    while(WiFi.status() != WL_CONNECTED){
     delay(100);
     Serial.println("connecting to WiFi...");
     }
  Serial.println("Connected to the WiFi network");
  Serial.println(WiFi.localIP());
 // Construct the API request URL
 String url = "http://api.openweathermap.org/data/2.5/weather?q=Tampa,FL,US&appid=" + apiKey;
 // Create an HTTPClient object
  HTTPClient http;
    // Begin the request
  http.begin(url);
    // Send the request
  int httpCode = http.GET();
    // Check the returning http code
  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(payload);
  } else {
    Serial.println("Error on HTTP request");
  }
  http.end(); //End HTTP connection
}
void loop() {
  // put your main code here, to run repeatedly:
}
