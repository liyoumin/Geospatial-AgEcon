#include <WiFi.h>

const char* ssid = "dana_sensor_network";
const char* password = "0123456789";
WiFiServer server(80);

String html = "<!DOCTYPE html> \
<html> \
<body> \
<center><h1>ESP32 Soft access point</h1></center> \
<center><h2>Web Server Hosting Test: Welcome to the Classroom!</h2></center> \
</body> \
</html>";

void setup() {
  Serial.begin(115200);
  Serial.print("Setting soft access point mode");
  WiFi.softAP(ssid, password);
  IPAddress IP = WiFi.softAPIP();
  Serial.print("AP IP address: ");
  Serial.println(IP);
  server.begin();
}

void loop() {
  WiFiClient client = server.available();
  if (client) {
    client.println("HTTP/1.1 200 OK");
client.println("Content-Type: text/html");
client.println("Connection: close");
client.println();
client.println(html);
  }
}
