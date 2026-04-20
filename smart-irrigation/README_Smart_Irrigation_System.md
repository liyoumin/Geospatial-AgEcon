# Smart Irrigation System

A smart irrigation controller built with an **Arduino Mega 2560** and an **ESP32** for local irrigation control, live phone dashboard monitoring, OpenWeather data integration, and ThingSpeak cloud upload.

This project monitors soil moisture, air temperature, humidity, water level, irrigation volume, and pump status. The Arduino Mega 2560 controls the sensors, LCD, keypad, relay, and pump logic. The ESP32 receives live telemetry, hosts a local web dashboard at `192.168.4.1`, connects to home WiFi, retrieves OpenWeather data for Woodland, California, and uploads data to ThingSpeak.

---

## Project Features

- Soil moisture monitoring using an analog soil moisture sensor
- DHT11 temperature and humidity monitoring
- Water level monitoring through the ESP32 analog input
- Automatic irrigation control based on soil moisture thresholds
- Manual irrigation control using a 4x4 keypad
- Irrigation volume control in milliliters
- LCD1602 display for local system status
- ESP32 local WiFi dashboard for phone viewing
- Home WiFi connection for internet services
- OpenWeather API integration for Woodland, CA
- ThingSpeak cloud data upload
- Pump control using a 5V relay module

---

## System Architecture

```text
Soil Moisture Sensor  ---->
DHT11 Sensor          ---->  Arduino Mega 2560  ----> Relay ----> Water Pump
Keypad                ---->        |
LCD1602 Display       <----        |
                                  UART Serial
                                      |
                                      v
                                  ESP32
                         /             |              \
                 Local Web Page   OpenWeather API   ThingSpeak
                 192.168.4.1      Woodland, CA      Cloud Charts
```

The Arduino Mega 2560 is the main controller. It reads sensor data, applies irrigation logic, controls the relay and pump, displays values on the LCD1602, and sends telemetry to the ESP32. The ESP32 handles WiFi, web dashboard display, OpenWeather requests, and ThingSpeak upload.

---

## Hardware Components

| Component | Function |
|---|---|
| Arduino Mega 2560 | Main irrigation controller |
| ESP32 Dev Module | WiFi, web dashboard, OpenWeather, ThingSpeak |
| Soil Moisture Sensor V2.0 | Measures soil water condition |
| DHT11 | Measures air temperature and relative humidity |
| Water Level Sensor | Measures water tank/reservoir level |
| LCD1602 | Displays local system status |
| 4x4 Keypad | Manual mode and volume control |
| 5V Relay Module | Switches pump power |
| Water Pump | Irrigation actuator |
| Push Button | Toggles AUTO/MANUAL mode |
| Voltage Divider | Protects ESP32 RX pin from Mega 5V serial signal |

---

## Arduino Mega 2560 Pin Connections

| Device | Mega2560 Pin |
|---|---|
| Soil moisture sensor signal | A0 |
| Water sensor signal | A1 |
| DHT11 data | D2 |
| Relay signal | D8 |
| Mode button | D3 |
| LCD RS | D22 |
| LCD E | D23 |
| LCD D4 | D24 |
| LCD D5 | D25 |
| LCD D6 | D26 |
| LCD D7 | D27 |
| Keypad rows | D30, D31, D32, D33 |
| Keypad columns | D34, D35, D36, D37 |

---

## ESP32 Pin Connections

| Device | ESP32 Pin | Notes |
|---|---|---|
| Mega TX serial output | GPIO16 RX2 | Use voltage divider from Mega TX to ESP32 RX |
| Mega RX serial input | GPIO17 TX2 | Optional if sending commands back to Mega |
| Water level sensor signal | GPIO34 | Analog input only |
| GND | GND | Common ground required |

### Important UART Note

The current Mega code uses:

```cpp
Serial.begin(9600);
Serial.print(...);
```

Therefore, without changing the Mega code, telemetry comes from **Mega Serial0**:

| Mega2560 | ESP32 |
|---|---|
| TX0 pin 1 | GPIO16 RX2 |
| RX0 pin 0 | GPIO17 TX2, optional |
| GND | GND |

If the Mega code is later changed to use `Serial1`, then the connection should move to Mega TX1/RX1 pins 18/19.

---

## Voltage Divider Warning

The Arduino Mega 2560 uses 5V logic, while the ESP32 uses 3.3V logic. Do **not** connect Mega TX directly to ESP32 RX.

Use a voltage divider, for example:

```text
Mega TX ---- 1kΩ ----+---- ESP32 GPIO16 RX2
                     |
                    2kΩ
                     |
                    GND
```

This reduces the 5V Mega serial signal to a safer voltage for the ESP32.

---

## Arduino Mega Telemetry Format

The Mega sends telemetry every 5 seconds in this format:

```text
ADC=630,Soil=44.0,T=23.0,RH=45.0,Pump=0,Mode=AUTO,LOW=30.0,HIGH=40.0,Vol=100,RunMS=4000
```

| Variable | Meaning |
|---|---|
| `ADC` | Raw soil moisture ADC value |
| `Soil` | Soil moisture percentage |
| `T` | DHT11 temperature in °C |
| `RH` | DHT11 relative humidity in % |
| `Pump` | Pump status, 1 = ON, 0 = OFF |
| `Mode` | AUTO or MAN |
| `LOW` | Auto irrigation ON threshold |
| `HIGH` | Auto irrigation OFF threshold |
| `Vol` | Target irrigation volume in mL |
| `RunMS` | Manual irrigation runtime in milliseconds |

---

## ThingSpeak Field Mapping

| ThingSpeak Field | Chart Name | Data Source |
|---|---|---|
| Field 1 | DHT11 Temperature C | Mega DHT11 `T` |
| Field 2 | Soil Moisture % | Mega soil sensor `Soil` |
| Field 3 | Water Level % | ESP32 water level sensor |
| Field 4 | OpenWeather Temp C | OpenWeather `main.temp` |
| Field 5 | Irrigation Volume mL | Mega `Vol` |
| Field 6 | Irrigation Status | Mega `Pump`, 1 = ON, 0 = OFF |
| Field 7 | DHT11 Humidity % | Mega DHT11 `RH` |
| Field 8 | OpenWeather Humidity % | OpenWeather `main.humidity` |

---

## ESP32 Local Dashboard

The ESP32 runs in **AP + STA mode**:

- AP mode: creates its own WiFi network for phone monitoring
- STA mode: connects to home WiFi for OpenWeather and ThingSpeak

Phone connection:

```text
WiFi Name: SmartIrrigation_ESP32
Password: 12345678
Browser: http://192.168.4.1
```

The local dashboard shows:

- DHT11 temperature
- Soil moisture
- Water level
- OpenWeather temperature
- Irrigation volume
- Pump status
- DHT11 humidity
- OpenWeather humidity
- Weather condition
- WiFi status
- ThingSpeak upload status
- Last Mega telemetry line

---

## Control Logic

### Automatic Mode

In automatic mode, the Mega reads soil moisture and compares it with threshold values.

```text
If soil moisture < LOW threshold:
    Pump ON

If soil moisture > HIGH threshold:
    Pump OFF
```

The code also adjusts thresholds during hot and dry conditions. If temperature is high and humidity is low, the system increases the irrigation sensitivity.

### Manual Mode

Manual mode allows the user to set irrigation volume using the keypad. The pump runtime is estimated from the pump flow rate:

```text
runtime = target volume / pump flow rate
```

The current pump flow setting is:

```cpp
const float PUMP_FLOW_ML_PER_SEC = 25.0;
```

---

## Keypad Operation

| Key | Function |
|---|---|
| Number keys | Enter irrigation volume |
| `#` | Save entered volume |
| `*` | Clear entry |
| `A` | Start manual irrigation |
| `B` | Stop pump |
| `C` | Force MANUAL mode |
| `D` | Force AUTO mode |

---

## LCD Display

The LCD1602 displays system mode, soil moisture, pump status, temperature, humidity, and target irrigation volume.

Example display:

```text
AUTO S:44% OFF
T:23C 45% v:100m
```

---

## OpenWeather Setup

The ESP32 requests current weather data for Woodland, California using latitude and longitude:

```text
Latitude: 38.6785
Longitude: -121.7733
```

The OpenWeather values used in this project are:

| OpenWeather JSON Field | Use |
|---|---|
| `main.temp` | ThingSpeak Field 4 |
| `main.humidity` | ThingSpeak Field 8 |
| `weather.main` | Dashboard status |
| `weather.description` | Dashboard status |

---

## Security Note

Do not upload private API keys to a public GitHub repository.

Use placeholders in the public code:

```cpp
const char* HOME_WIFI_SSID = "YOUR_HOME_WIFI_NAME";
const char* HOME_WIFI_PASS = "YOUR_HOME_WIFI_PASSWORD";
String apiKey = "YOUR_OPENWEATHER_API_KEY";
const char* THINGSPEAK_WRITE_API_KEY = "YOUR_THINGSPEAK_WRITE_API_KEY";
```

For a public repository, store real credentials in a separate local file such as `secrets.h` and add it to `.gitignore`.

---

## Required Arduino Libraries

Install these libraries in Arduino IDE:

### Arduino Mega 2560

- `DHT sensor library`
- `LiquidCrystal`
- `Keypad`

### ESP32

- `WiFi`
- `WebServer`
- `HTTPClient`

The ESP32 board package must also be installed in Arduino IDE.

---

## Upload Procedure

### Arduino Mega 2560

1. Select board: `Arduino Mega or Mega 2560`
2. Select the correct COM port
3. Upload the Mega irrigation control code
4. Open Serial Monitor at `9600 baud`
5. Confirm telemetry appears every 5 seconds

Example:

```text
ADC=630,Soil=44.0,T=23.0,RH=45.0,Pump=0,Mode=AUTO,LOW=30.0,HIGH=40.0,Vol=100,RunMS=4000
```

### ESP32

1. Select board: `ESP32 Dev Module`
2. Select the correct COM port
3. Upload the ESP32 web/cloud code
4. Open Serial Monitor at `115200 baud`
5. Connect phone to `SmartIrrigation_ESP32`
6. Open `http://192.168.4.1`

---

## Troubleshooting

### No data on ESP32 dashboard

- Check common GND between Mega and ESP32
- Check Mega TX to ESP32 GPIO16 wiring
- Check voltage divider
- Confirm Mega Serial Monitor shows telemetry
- Confirm ESP32 code baud rate is `9600`

### ESP32 uploads fail

- Check home WiFi SSID and password
- Check ThingSpeak Write API Key
- Wait at least 15–20 seconds between uploads
- Confirm ESP32 has internet access

### OpenWeather does not show data

- Check OpenWeather API key
- Confirm home WiFi is connected
- Confirm API request is not blocked
- OpenWeather values update slower than local sensor values

### Mega upload fails

If ESP32 is connected to Mega pins 0 and 1, unplug those wires before uploading code to the Mega. Pins 0 and 1 are shared with the Mega USB serial upload interface.

---

## Project Purpose

This smart irrigation system demonstrates how sensor-based irrigation control and IoT monitoring can improve water management in agricultural production. It is especially relevant for semi-arid crop systems such as alfalfa, where soil moisture, temperature, humidity, and water availability are important for irrigation scheduling.

---

## Repository Structure

```text
Smart-Irrigation-System/
│
├── README.md
├── mega2560_irrigation/
│   └── mega2560_irrigation.ino
├── esp32_dashboard_thingspeak/
│   └── esp32_dashboard_thingspeak.ino
├── docs/
│   ├── wiring_diagram.png
│   └── system_architecture.png
└── .gitignore
```

---

## Author

Youmin Li  
Smart Agriculture and IoT Project  
University of Florida
