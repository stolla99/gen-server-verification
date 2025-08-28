<a id="readme-top"></a>
<div align="center">
  <a href="https://github.com/othneildrew/Best-README-Template">
    <img src="images/logo.png" alt="Logo" width="80" height="80">
  </a>
  <h3 align="center">Batch Scripts</h3>
<p><br /></p>
</div>

This repository contains **batch scripts** for running and managing TLC model checker runs for TLA+ specifications. The scripts allow you to run TLC with symmetry options, cancel running TLC processes, and save snapshots of TLC runs with timestamps.

## Overview
<table>
  <tr>
    <th>Script</th>
    <th>Details</th>
  </tr>
  <tr>
    <td><code>./run.sh</code></td>
    <td>
        <p>Runs TLC with symmetry (<code>-symm</code>) for specified modules.</p>
        <h5>Usage</h5>
        <pre><code>./run.sh &lt;options&gt;</code></pre>
        <h5>Options</h5>
        <p><strong>Available options:</strong> <code>S</code>, <code>SE</code>, <code>P</code>, <code>none</code></p>
        <ul>
            <li><code>none</code> runs TLC for all modules.</li>
            <li>You can list one or multiple modules to run.</li>
        </ul>
        <h5>Examples</h5>
        <pre><code>./run.sh S       # Runs TLC for module S
./run.sh SE P    # Runs TLC for modules SE and P
./run.sh -symm   # Runs TLC with symmetry enabled for all modules</code></pre>
    </td>
  </tr>
  <tr>
    <td><code>./cancel.sh</code></td>
    <td>
        <p>Stops TLC processes for specified modules.</p>
        <h5>Usage</h5>
        <pre><code>./cancel.sh &lt;options&gt;</code></pre>
        <h5>Options</h5>
        <p><strong>Available options:</strong> <code>S</code>, <code>SE</code>, <code>P</code>, <code>T</code></p>
        <ul>
            <li>Provide one, multiple, or all options to stop TLC for the corresponding modules.</li>
        </ul>
        <h5>Example</h5>
        <pre><code>./cancel.sh S SE</code></pre>
        <p>Stops TLC processes for <code>S</code> and <code>SE</code>.</p>
    </td>
</tr>
<tr>
    <td><code>./snapshot.sh</code></td>
    <td>
        <p>Saves snapshot into a specific folder wiht timestamp.</p>
        <h5>Usage</h5>
        <pre><code>./snapshot.sh &lt;options&gt;</code></pre>
        <h5>Options</h5>
        <p><strong>Available options:</strong> <code>S</code>, <code>SE</code>, <code>P</code></p>
        <ul>
            <li>You can list one or multiple modules to be snapshot after TLC has finished.</li>
        </ul>
        <h5>Examples</h5>
        <pre><code>./snapshot.sh S       # Runs TLC for module S
./snapshot.sh SE P    # Runs TLC for modules SE and P</code></pre>
    </td>
</tr>
</table>
<p align="right">(<a href="#readme-top">↑ back to top</a>)</p>