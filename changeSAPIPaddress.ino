#include <WiFi.h>

const char* ssid = "SmartIrrigation_Youmin";
const char* password = "12345678";

// Set your Static IP address for the Soft Access Point
IPAddress local_IP(192, 168, 125, 1);  // The common default IP for ESP32 SAP is 192.168.4.1
// Set your Gateway IP address (usually same as local_IP for SAP)
IPAddress gateway(192, 168, 125, 1);
// Set your Subnet mask
IPAddress subnet(255, 255, 255, 0);

WiFiServer server(80);
String html = "<!DOCTYPE html> \
<html> \
<body> \
<center><h1>ESP32 Soft access point</h1></center> \
<center><h2>Web Server Testing: Welcome to the Classroom!</h2></center> \
</body> \
</html>";

void setup() {
  Serial.begin(115200);
  Serial.print("Setting soft access point mode");

  if (!WiFi.softAPConfig(local_IP, gateway, subnet)) {
    Serial.println("SoftAPConfig has failed!");
  } else {
    Serial.println("SoftAPConfig has been successful!");
  }

  WiFi.softAP(ssid, password);
  IPAddress IP = WiFi.softAPIP();
  Serial.print("AP IP address: ");
  Serial.println(IP);
  server.begin();
}

void loop() {
  WiFiClient client = server.available();
  if (client) {
    client.print(html);
  }
}
