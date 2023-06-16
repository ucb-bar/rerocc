# ReRoCC: Remote RoCC Extension for RoCC-enabled RISC-V Cores

The Remote RoCC (ReRoCC) extensions for RoCC-enabled RISC-V cores enables physical disaggregation of RoCC accelerators from control cores.
To simplify adoption and exploration, ReRoCC is backwards compatible with existing RoCC accelerator implementations and software stacks.
ReRoCC provides a path towards supporting "accelerator virtualization" on many-accelerator architectures, where threads can contend for and share limited physical accelerator resources without requiring user-level software modifications.

## ReRoCC Accelerator Client

The accelerator [client](https://github.com/ucb-bar/rerocc/blob/master/src/main/scala/Client.scala) attaches to existing RISC-V cores to enable them to use remote ReRoCC accelerators.
The client is itself a RoCC "accelerator", and can plug-in to existing cores like Rocket, (TODO) BOOM, and (TODO) Shuttle.

The client is responsible for tracking which physical accelerators are acquired by the attached hart, forwarding accelerator instructions, and maintaining shadowed architectural state on the remote accelerators.

## ReRoCC Accelerator Manager

The accelerator [manager](https://github.com/ucb-bar/rerocc/blob/master/src/main/scala/Manager.scala) wraps an existing RoCC accelerator implementation, providing the standard RoCC interface to the accelerator.
The manager implements shadow copies of critical core architectural state, including the ``mstatus`` and ``ptbr`` registers, as well as a shadow page-table-walker, for the accelerator to access.

## ReRoCC RISC-V Architectural Extensions

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

| ReRoCC CSR Field | Description                                                                                                                                                     |
|------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ``rropcX[3:0]``  | Index of ``rrcfg`` opcode ``customX`` is mapped to                                                                                                              |
| ``rrbar[3:0]``   | When written, indicates that the next memory fence should apply to accelerator managed by the indexed ``rrcfg``                                                 |
| ``rrcfgX[7:0]``  | Index of physical accelerator this register is mapped to                                                                                                        |
| ``rrcfgX[8]``    | If set, indicates this register should attempt to "acquire" the corresponding physical accelerator. Reading this bit returns whether acquisition was successful |

## ReRoCC Messaging Protocol

The ReRoCC messaging protocol is a simple, in-order, non-blocking, dual-channel packetized messaging protocol to facilitate communication and synchronization between accelerator clients and managers.
A ``request`` channel is for client-to-manager traffic, while a ``response`` channel implements manager-to-client traffic.
Both channels use the same message encoding.

The protocol can be implemented by a crossbar or NoC-based interconnect.
The protocol implements a 64-bit data channel with multi-beat message types.

### ReRoCC Message Fields

| Field      | Width | Description                                                                                                          |
|------------|-------|----------------------------------------------------------------------------------------------------------------------|
| opcode     | 3     | Type of the message. See the opcode table.                                                                           |
| client_id  | ?     | ID of the ``rrcfg`` register on one end of the message. Managers see unique IDs per ``rrcfg`` on a multi-hart system |
| manager_id | ?     | ID of the physical manager on one end of the message.                                                                |
| data       | 64    | See opcode table                                                                                                     |

### Request Channel Opcodes/Data encodings

| ``Opcode`` | Value     | Total Beats | Beat | ``data[63:0]``                  |
|------------|-----------|-------------|------|---------------------------------|
| mAcquire   | ``0b000`` | 1           | 0    | Ignored                         |
| mInst      | ``0b001`` | 1-3         | 0    | 32-bit custom instruction       |
|            |           |             | 1    | (If requested) rs1 operand data |
|            |           |             | 2    | (If requested) rs2 operand data |
| mUStatus   | ``0b010`` | 2           | 0    | New ``csr_mstatus[63:0]``       |
|            |           |             | 1    | New ``csr_mstatus[127:64]``     |
| mUPtbr     | ``0b011`` | 1           | 0    | New ``csr_ptbr``                |
| mRelease   | ``0b100`` | 1           | 0    | Ignored                         |
| mUnbusy    | ``0b101`` | 1           | 0    | Ignored                         |

### Response Channel Opcodes/Data encodings

| ``Opcode`` | Value     | Total Beats | Beat | ``data[63:0]``                            |
|------------|-----------|-------------|------|-------------------------------------------|
| sAcqResp   | ``0b000`` | 1           | 0    | ``1`` if successful acquire, else ``0``   |
| sInstAck   | ``0b001`` | 1           | 0    | Ignored                                   |
| sWrite     | ``0b010`` | 2           | 0    | 64-bit register write data                |
|            |           |             | 1    | 64-bit architectural register destination |
| sRelResp   | ``0b011`` | 1           | 0    | Ignored                                   |
| sUnbusyAck | ``0b100`` | 1           | 0    | Ignored                                   |

### Message Flows

| Request Message | Response Message | Description                                                                                      |
|-----------------|------------------|--------------------------------------------------------------------------------------------------|
| mAcquire        | mAcqResp         | Attempt to acquire a ReRoCC-managed accelerator, manager responds whether acquire was successful |
| mInst           | mInstAck         | Sends an instruction stream to the manager, credit-based flow-control with Inst/InstAck          |
| mUStatus        |                  | Updates shadowed MStatus on the manager                                                          |
| mUPtbr          |                  | Updates shadowed satp on the manager                                                             |
| mRelease        | mRelResp         | Relinquishes an attached accelerator                                                             |
| mUnbusy         | mUnbusyAck       | Wait for accelerator to unbusy (TODO: Remove)                                                    |

## Status

ReRoCC is currently under development. A brief list of TODOs:

 * Support BOOM/Shuttle cores, these cores need to support RoCC-provided CSRs
 * Upstream necessary rocket-chip APIs
 * Support supervisor-level accelerator management
 * Remove need for explicit rerocc memory fence via ``rrbar`` csr.
