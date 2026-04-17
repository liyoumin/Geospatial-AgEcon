#define TS_ENABLE_SSL

#include <WiFi.h>
#include <WiFiClientSecure.h>
#include <DHT.h>
#include <ThingSpeak.h>

const char* ssid = "iPhone Youmin li";
const char* password = "12345678";

WiFiClientSecure client;

unsigned long myChannelID = 3347147;
const char * myWriteAPIKey = "V85NE5L7FEY0ZIUU";

// Timer variables
unsigned long lastTime = 0;
unsigned long timerDelay = 30000;

#define DHTPIN 23
#define DHTTYPE DHT11 
DHT dht(DHTPIN, DHTTYPE);

float temperature;

void setup() {
  // put your setup code here, to run once:
  Serial.begin(115200);  //Initialize serial
  WiFi.begin(ssid, password);
      while(WiFi.status() != WL_CONNECTED){
       delay(100);
       Serial.println("connecting to WiFi...");
       }
  Serial.println("Connected to the WiFi network");
  Serial.println(WiFi.localIP()); 
  ThingSpeak.begin(client);  // Initialize ThingSpeak
  dht.begin();
}


void getData(){
  delay(2000);// Wait a few seconds between measurements.
  // Reading temperature or humidity takes about 250 milliseconds!
  // Sensor readings may also be up to 2 seconds ‘old’ (its a very slow sensor)
    temperature = dht.readTemperature();// Read temperature as Celsius (the default)
    if (isnan(temperature) ){
    Serial.println("Failed to read from DHT sensor.");
    return;
     }
  Serial.print("Temperature (°C): ");
  Serial.println(temperature);
}


void loop() {
  // put your main code here, to run repeatedly:
if ((millis() - lastTime) > timerDelay) {
  getData();
   int x = ThingSpeak.writeField(myChannelID, 1, temperature, myWriteAPIKey);
   if(x == 200){
      Serial.println("Channel update successful.");
    }
    else{
      Serial.println("Problem updating channel. HTTP error code " + String(x));
    }
    lastTime = millis();
  }
}
