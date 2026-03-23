# SML Assembly Interpreter 

Attempt at a simple assembly

## Features (currently)
- **Labels**
- **Jumps**
- **Memory**
- **Registers**

---

## 🛠 Instruction Set Architecture

### Memory & Registers
- **Registers:** Referenced as `r1`, `r2`, etc. (Note: This interpreter uses **1-based indexing**).
- **Memory:** Referenced as `m100`, `m200`, etc.

### Core Instructions
| Category | Mnemonics | Description |
| :--- | :--- | :--- |
| **Data** | `LOAD`, `STOREV`, `STORER` | Move data between registers and memory. |
| **Math** | `ADDxxx`, `SUBxxx` | Addition and Subtraction. Suffixes (R, V, M) indicate Register, Value, or Memory. |
| **Flow** | `JMP`, `JZ`, `JNZ` | Jump, Jump if Zero, Jump if Not Zero. |
| **Misc** | `LABEL`, `PRINT`, `NOP` | Define jump points, output register values, or do nothing. |

---

## How to use

### 1. Loading the Interpreter
```sml use "interpreter.sml";```

### 2. Running the interpreter
```sml Interpreter.runFile "path/to/your_code.asm";```

### Example code
```assembly
STOREV  m1, 10
LOAD    m1, r1
ADDVV   r2, 0, 0

LABEL LOOP
ADDRR   r2, r2, r1
SUBRV   r1, r1, 1
JNZ     r1, LOOP

PRINT   r2
```