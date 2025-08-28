<a id="readme-top"></a>
<!-- PROJECT LOGO -->
<br />
<div align="center">
  <p>🔩📜🧩🧪🪒</p>

  <h3 align="center">Master Thesis: Modeling and Verifying Erlang's GenServer with TLA+</h3>

  <p align="center">
    <a href="https://github.com">View Demo</a>
    &middot;
    <a href="https://github.com">Report Bug</a>
    &middot;
    <a href="https://github.com">Request Feature</a>
  </p>
</div>

<p align="center">
  <img src="https://img.shields.io/badge/Java-%23ED8B00.svg?logo=openjdk&logoColor=white" />
  <img src="https://img.shields.io/badge/Bash-4EAA25?logo=gnubash&logoColor=fff" />
  <img src="https://img.shields.io/badge/Erlang-A90533?logo=erlang&logoColor=fff" />
  <img src="https://img.shields.io/badge/TLA+-T" />
  <img src="https://img.shields.io/badge/PlusCal-8A2BE2" />
</p>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#repository-structure">Repository Structure</a>
    </li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project

[![Product Name Screen Shot][product-screenshot]](https://example.com)


This repository contains the artifacts generated and used during the work on my Master’s thesis. It includes custom modules, automatically generated Erlang code, test results, translation code, and batch scripts. 

A formal TLA⁺/PlusCal specification of the Erlang/OTP GenServer was developed and verified through model checking, and this specification was extended with a translation pipeline that automatically generates correct and usable Erlang GenServer code, whichreadme successfully passed the official OTP test suite and additional QuickCheck tests.  

<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>

## Repository Structure
<table>
  <tr>
    <th>Subfolder</th>
    <th>Explanation</th>
    <th>Link</th>
  </tr>
  <tr>
    <td><code>custom-module/</code></td>
    <td>Contains manually written Erlang modules that complement or extend the automatically generated GenServer code.</td>
    <td><a href="./custom-module">custom-module</a></td>
  </tr>
  <tr>
    <td><code>gen-server-out/</code></td>
    <td>Contains the <b>automatically generated Erlang files</b> (GenServer implementation). These files are generated from PlusCal specifications and should not be manually modified.</td>
    <td><a href="./gen-server-out">gen-server-out</a></td>
  </tr>
  <tr>
    <td><code>gen-server-suite-results/</code></td>
    <td>Contains the <b>Common Test (CT) logs and results</b> from executing the generated GenServer test suite. Mainly for validation and traceability.</td>
    <td><a href="./gen-server-suite-results">gen-server-suite-results</a></td>
  </tr>
  <tr>
    <td><code>slurm-scripts/</code></td>
    <td>Contains <b>batch scripts for SLURM</b> to execute TLC model checking in a cluster environment. These scripts handle automated runs of the verification process.</td>
    <td><a href="./slurm-scripts">slurm-scripts</a></td>
  </tr>
  <tr>
    <td><code>translation/</code></td>
    <td>Contains the <b>Java implementation</b> responsible for automatically extracting Erlang code from PlusCal specifications. This is the translation layer between formal specification and executable Erlang.</td>
    <td><a href="./translation">translation</a></td>
  </tr>
</table>

<!-- CONTACT -->
## Contact

 - Arne Stoll - arne.stoll.1@outlook.de 
 - Repository-Link: [stolla99/gen-server-verification](https://github.com/stolla99/gen-server-verification#)
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>

## Credits
[othneildrew/Best-README-Template](https://github.com/othneildrew/Best-README-Template/blob/main/README.md)
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>

