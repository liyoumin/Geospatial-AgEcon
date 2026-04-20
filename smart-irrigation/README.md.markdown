# Automatic Irrigation System Using Arduino Uno

[![Arduino](https://img.shields.io/badge/Arduino-Uno-00979D?style=for-the-badge&logo=Arduino&logoColor=white)](https://www.arduino.cc/) 
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg?style=for-the-badge)](https://opensource.org/licenses/MIT) 
[![CircuitDigest](https://img.shields.io/badge/Tutorial-CircuitDigest-blue?style=for-the-badge)](https://circuitdigest.com/microcontroller-projects/automatic-irrigation-system-using-arduino-uno)

A DIY **Automatic Irrigation System** project using Arduino Uno and a soil moisture sensor to automatically water plants based on soil moisture levels, ensuring healthy plants even during extended absences.

![Automatic Irrigation System](https://circuitdigest.com/sites/default/files/projectimage_mic/Arduino-Automatic-Irrigation-System.jpg)


## 🚀 Features

- **Automated Watering** - Triggers water pump based on soil moisture levels
- **Soil Moisture Detection** - Real-time monitoring using a soil moisture sensor
- **Relay Control** - Manages pump operation safely and efficiently
- **Low Maintenance** - Operates independently without human intervention
- **Customizable Thresholds** - Adjustable moisture levels for different plants
- **Cost-Effective** - Affordable setup for home gardens or indoor plants
- **Portable Design** - Compact system for versatile deployment
- **Suitable for Indoor/Outdoor** - Works for both garden and potted plants

## 🛠️ Hardware Requirements

### Primary Components

- **Arduino Uno** (1x) - Microcontroller for processing and control
- **Soil Moisture Sensor** (1x) - Measures soil moisture levels
- **5V Relay Module** (1x) - Controls the water pump
- **6V Mini Water Pump with Pipe** (1x) - Delivers water to plants
- **5V Battery** (1x) - Power source for the system
- **Connecting Wires** - For circuit connections
- **Breadboard** (Optional) - For prototyping connections

### Optional Components

- **LCD Display** - For real-time moisture level display
- **LED Indicators** - Visual feedback for system status
- **External Power Supply** - For larger pumps or extended operation
- **Water Container** - Reservoir for the pump

## 📐 Circuit Diagram

```
Arduino Uno Connections:
┌─────────────────┬──────────────────┬────────────────┐
│ Arduino Pin     │ Component        │ Function       │
├─────────────────┼──────────────────┼────────────────┤
│ A0             │ Soil Moisture    │ Analog Input   │
│ 3              │ Relay Module     │ Signal Control │
│ 5V, GND        │ Relay/Sensor     │ Power Supply   │
│ Vin, GND       │ Battery          │ Power Input    │
└─────────────────┴──────────────────┴────────────────┘
```

**📖 Complete Circuit Diagram:** [View Detailed Schematic](https://circuitdigest.com/fullimage?i=/circuitdiagram_mic/Automatic-Irrigation-System-Circuit-Diagram.jpg)

## ⚙️ Installation

### 1. Arduino IDE Setup

1. Install the Arduino IDE from [arduino.cc](https://www.arduino.cc/en/software)
2. Connect the Arduino Uno to your computer via USB
3. Update the Arduino board definitions:
   ```
   Tools -> Board -> Boards Manager -> Install Arduino AVR Boards
   ```

### 2. Software Dependencies

No external libraries are required for this project. The code uses basic Arduino functions.

### 3. Code Setup

Clone the project repository:
```
git clone https://github.com/electroscopearchive/automatic-irrigation-system-arduino-uno.git
cd automatic-irrigation-system-arduino-uno
```

Upload the main script using Arduino IDE:
```
File -> Open -> code.ino
```

## 🎯 Usage

1. Connect the soil moisture sensor, relay module, and water pump to the Arduino Uno as per the circuit diagram
2. Upload the Arduino code (`code.ino`)
3. Insert the soil moisture sensor into the plant soil
4. Place the pump in a water container with the pipe directed to the plant
5. Power the system with a 5V battery
6. Monitor the system via Serial Monitor (optional) for debugging

### Sample Code

```cpp
int soilMoistureValue = 0;
int percentage = 0;

void setup() {
  pinMode(3, OUTPUT);
  Serial.begin(9600);
}

void loop() {
  soilMoistureValue = analogRead(A0);
  percentage = map(soilMoistureValue, 490, 1023, 0, 100);
  Serial.println(percentage);
  
  if (percentage < 10) {
    Serial.println("pump on");
    digitalWrite(3, LOW);
  }
  if (percentage > 80) {
    Serial.println("pump off");
    digitalWrite(3, HIGH);
  }
}
```

## 📁 Code Structure

```
├── Code/
│   └── code.ino                    # Main Arduino sketch
├── Documentation/
│   └── Component_Connections.md     # Detailed connections
├── Circuit Diagram/
│   └── Irrigation_System_Circuit.png # Wiring schematic
└── README.md                       # This file
```

### Key Functions

- `analogRead()` - Reads soil moisture sensor data
- `map()` - Converts raw sensor values to percentage
- `digitalWrite()` - Controls the relay to turn the pump on/off
- `Serial.println()` - Outputs debug information to Serial Monitor

## 🔧 Troubleshooting

### Common Issues

**Sensor Not Reading Correctly**
- Verify sensor connections to A0, 5V, and GND
- Calibrate sensor using dry and wet soil values
- Check for loose wires or damaged sensor

**Pump Not Activating**
- Ensure relay module is connected to pin 3 and powered
- Verify battery voltage matches pump requirements
- Check relay signal (active LOW)

**Inconsistent Watering**
- Adjust moisture thresholds (10% and 80%) in code
- Ensure sensor is placed near plant roots
- Test pump with a manual toggle to confirm functionality

## 📱 Applications

### Home Gardening
- **Indoor Plants** - Automate care for potted plants
- **Small Gardens** - Maintain backyard or balcony gardens
- **Vacation Care** - Water plants during extended absences

### Agriculture
- **Greenhouses** - Manage irrigation for controlled environments
- **Small Farms** - Automate watering for small crop zones
- **Hydroponics** - Regulate water delivery in soilless systems

### Education
- **STEM Projects** - Teach microcontroller and sensor integration
- **Plant Science** - Study moisture effects on plant growth
- **Automation Lessons** - Demonstrate feedback control systems

## 🔮 Future Enhancements

- [ ] **WiFi Connectivity** - Add ESP8266 for remote monitoring
- [ ] **Mobile App Integration** - Control and monitor via smartphone
- [ ] **Multiple Sensors** - Support multiple plants with zoned irrigation
- [ ] **LCD Display** - Show real-time moisture and pump status
- [ ] **Solar Power** - Use solar panels for eco-friendly operation
- [ ] **Data Logging** - Store moisture data for analysis

## 🏗️ Technical Specifications

| Parameter             | Value                     |
|-----------------------|---------------------------|
| Operating Voltage     | 5V DC                     |
| Current Consumption   | 1-3W (with pump active)   |
| Sensor Range          | 0-100% moisture           |
| Pump Capacity         | 100-500mL/min (adjustable)|
| Operating System      | Arduino IDE               |
| Operating Temperature | 0°C to 50°C               |

## 🤝 Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit changes (`git commit -m 'Add AmazingFeature'`)
4. Push to branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Contributing Guidelines

- Follow Arduino coding standards
- Test modifications on Arduino hardware
- Update documentation for new features
- Include comments for complex code sections

## 🔗 Links

- **📖 Complete Tutorial**: [CircuitDigest - Automatic Irrigation System Guide](https://circuitdigest.com/microcontroller-projects/automatic-irrigation-system-using-arduino-uno)
- **📚 Arduino Soil Moisture Sensor Tutorial**: [Circuit Digest](https://circuitdigest.com/microcontroller-projects/interfacing-soil-moisture-sensor-with-arduino-uno)
- **🎓 More Arduino Projects**: [Circuit Digest Arduino Collection](https://circuitdigest.com/simple-arduino-projects-for-beginners)

## ⭐ Support

If this project helped you, please ⭐ **star this repository** and share it with others!

---

**Built with ❤️ by [Circuit Digest](https://circuitdigest.com/) | Making Electronics Simple**

---

### Keywords

`arduino irrigation system` `soil moisture sensor project` `diy automatic watering` `arduino uno projects` `smart irrigation system` `plant care automation` `relay module control` `arduino gardening projects` `automated plant watering`
