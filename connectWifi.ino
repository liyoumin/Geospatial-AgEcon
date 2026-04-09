#include <WiFi.h>

const char* ssid = "iPhone Youmin li";
const char* password = "12345678";

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
 }

void loop() {
  // put your main code here, to run repeatedly:
    while(WiFi.status() != WL_CONNECTED){
     delay(100);
     Serial.println("connecting to WiFi...");
     }
     
     Serial.println("Connected to the WiFi network");
     Serial.println(WiFi.localIP());
}
