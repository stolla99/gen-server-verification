<a id="readme-top"></a>
<a id="readme-top"></a>
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>
  <h3 align="center">Custom Modules</h3>
<p><br /></p>
</div>

This folder contains **custom operators for TLA⁺**, implemented in Java. These operators extend the functionality of TLA⁺ specifications and can be integrated into the TLA⁺ IDE (TLA Toolbox) through the override mechanism.  

## Purpose
- Provide reusable **custom operators** for TLA⁺ that can be referenced in specifications.  
- Allow tighter integration of specific logic into the **TLC model checker**.  
- Serve as an extension point beyond the built-in operators of TLA⁺.  
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>

## Build and Override Operations

To build and incorporate the custom operators into the TLA⁺ IDE:

1. **Build with Java 13 (or lower)**  
  - Ensure the class version is **58 or smaller** (Java 13 corresponds to version 57/58).

2. **Place the classes in the TLC overrides directory**  
  - Add them to the `Override` class (typically in `tlc2/overrides/TLCOverrides.java`).  
  ```Java
  return new Class[] { Bitwise.class, Typecheck.class, Timeouts.class, Encoder.class };
  ```

3. **Remove the `META-INF` directory**  
  - Ensure that the JAR or compiled classes do not contain `META-INF`.  
  - This avoids `SecurityException` issues when TLC loads the classes.  

4. **Import into Toolbox Preferences**  
  - Open **TLA Toolbox → Preferences → TLA+ Preferences → Add Archive File**.  
  - Add your compiled JAR or classpath entry.  
  - Or via command line as follows (```-cp file-path class.Class```):
  ```shell
    java -XX:+UseParallelGC -Xmx180g -Djava.awt.headless=true 
      -cp ./../../toolbox/tla2tools.jar tlc2.TLC 
      -config ./../$@/P_config.cfg 
      -checkpoint 0 
      -lncheck final 
      -workers 32 
      -fpmem 1.0 -gzip gen_server_behaviour_simple.tla
   ```
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>

## Usage in TLA⁺
Once the module is built and registered in the TLA Toolbox, the custom operators can be directly used in your `.tla` specifications as if they were native operators. 

Example (assuming an operator `ComplexOperation` is defined in Java):  
```tla
---- MODULE Example ----
EXTENDS CustomModule

VARIABLE x

Init == x = ComplexOperation(42)
Next == x' = ComplexOperation(x)

==== 
```
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>
