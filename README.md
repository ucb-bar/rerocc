# ReRoCC: Remote RoCC Extension for RoCC-enabled RISC-V Cores

The Remote RoCC (ReRoCC) extensions for RoCC-enabled RISC-V cores enables physical disaggregation of RoCC accelerators from control cores.
To simplify adoption and exploration, ReRoCC is backwards compatible with existing RoCC accelerator implementations and software stacks.
ReRoCC provides a path towards supporting "accelerator virtualization" on many-accelerator architectures, where threads can contend for and share limited physical accelerator resources without requiring user-level software modifications.

## ReRoCC Architectural Extensions

ReRoCC adds new user-level CSRs to the host CPU, which manage the availability of physical accelerators for the CPU thread.

The ``rrcfg`` registers track an "acquired" accelerator available for the hart to access.
Implementations may support up to 16 ``rrcfg`` registers.
Each ``rrcfg`` register contains a ``mgr`` field, which can be set to the physical ID of a remote RoCC accelerator manager, and a ``acq`` field, which, if set, indicates that the accelerator is available to access.

The ``rropc`` registers assign ``rrcfg`` registers to the four available RoCC custom opcodes.
When a ``rropcX`` register is set to the index of a ``rrcfg`` register with the ``acq`` bit set, then instructions with opcode ``customX`` will be send to that accelerator.

The ``rrbar`` register is currently used to assist in fencing accelerator memory accesses. This will be removed in the future.

| CSR Id      | Name               |
|-------------|--------------------|
| 0x800-0x803 | ``rropc0-rropc3 `` |
| 0x804       | ``rrbar         `` |
| 0x810-0x81f | ``rrcfg0-rrcfg15`` |

## ReRoCC Accelerator Client

The accelerator client attaches to existing RISC-V cores to enable them to use remote ReRoCC accelerators.
The client is itself a RoCC "accelerator", and can plug-in to existing cores like Rocket, (TODO) BOOM, and (TODO) Shuttle.

## ReRoCC Accelerator Manager

## ReRoCC Messaging Protocol

## ReRoCC Example

## Status

ReRoCC is currently under development. A brief list of TODOs:

 * Support BOOM/Shuttle cores, these cores need to support RoCC-provided CSRs
 * Upstream necessary rocket-chip APIs
 * Support supervisor-level accelerator management
