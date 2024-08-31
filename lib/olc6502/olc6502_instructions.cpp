#include "olc6502.h"
#include "common.h"

///////////////////////////////////////////////////////////////////////////////
// ADDRESSING MODES

// The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
// to as the "page", and the low byte is the offset into that page. This implies
// there are 256 pages, each containing 256 bytes.
//
// Several addressing modes have the potential to require an additional clock
// cycle if they cross a page boundary. This is combined with several instructions
// that enable this additional clock cycle. So each addressing function returns
// a flag saying it has potential, as does each instruction. If both instruction
// and address function return 1, then an additional clock cycle is required.


// Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
u8 olc6502::IMP() {
    fetched = a;
    return 0;
}

// Address Mode: Immediate
// the addr_abs temp is put on pc, and pc is incremented to next byte.
u8 olc6502::IMM() {
    addr_abs = pc++;
    return 0;
}

// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
u8 olc6502::ZP0() {
    addr_abs = read(pc++);
    addr_abs &= 0x00ff;
    return 0;
}

// Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
u8 olc6502::ZPX() {
    addr_abs = read(pc++) + x;
    addr_abs &= 0x00ff;
    return 0;
}

// Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
u8 olc6502::ZPY() {
    addr_abs = read(pc++) + y;
    addr_abs &= 0x00ff;
    return 0;
}

// Address Mode: Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
u8 olc6502::REL() {
    addr_rel = read(pc++);
    if (addr_rel & 0x80) {
        addr_rel |= 0xff00;
    }
    return 0;
}

// Address Mode: Absolute
// A full 16-bit address is loaded and used
u8 olc6502::ABS() {
    u16 lo = read(pc++);
    u16 hi = read(pc++);
    addr_abs = (hi << 8) | lo;
    return 0;
}

// Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
u8 olc6502::ABX() {
    u16 lo = read(pc++);
    u16 hi = read(pc++);
    addr_abs = ((hi << 8) | lo) + x;
    if ((addr_abs & 0xff00) != (hi << 8)) {
        return 1;
    }
    return 0;
}

// Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
u8 olc6502::ABY() {
    u16 lo = read(pc++);
    u16 hi = read(pc++);
    addr_abs = ((hi << 8) | lo) + y;
    if ((addr_abs & 0xff00) != (hi << 8)) {
        return 1;
    }
    return 0;
}

// Note: The next 3 address modes use indirection (aka Pointers!)

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
u8 olc6502::IND() {
    u16 ptr_lo = read(pc++);
    u16 ptr_hi = read(pc++);
    u16 ptr = (ptr_hi << 8) | ptr_lo;
    if (ptr_lo == 0x00FF) {
        addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
    } else {
        addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
    }
    return 0;
}

// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
u8 olc6502::IZX() {
    u16 ptr = (u16) read(pc++) + (u16) x;
    u16 lo = read(ptr & 0x00ff);
    ptr++;
    u16 hi = read(ptr & 0x00ff);
    addr_abs = (hi << 8) | lo;
    return 0;
}

// Address Mode: Indirect Y
// The supplied 8-bit address is offset by Y Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
u8 olc6502::IZY() {
    u16 ptr = (u16) read(pc++) + (u16) y;
    u16 lo = read(ptr & 0x00ff);
    ptr++;
    u16 hi = read(ptr & 0x00ff);
    addr_abs = (hi << 8) | lo;
    return 0;
}


// the address mode prepares the addresse depending on the mode in the variable addr_abs.
// and the fetch fetches the value pointed by the addr_abs only if it's not IMM
u8 olc6502::fetch() {
    if (lookup[opcode].addrmode != &olc6502::IMP) {
        fetched = read(addr_abs);
    }
    return fetched;
}


///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

u8 olc6502::ADC() {
    // Grab the data that we are adding to the accumulator
    fetch();

    // Add is performed in 16-bit domain for emulation to capture any
    // carry bit, which will exist in bit 8 of the 16-bit word
    temp = (u16) a + (u16) fetched + (u16) getFlag(C);

    // The carry flag out exists in the high byte bit 0
    setFlag(C, temp > 255);

    // The Zero flag is set if the result is 0
    setFlag(Z, (temp & 0x00FF) == 0);

    // The signed Overflow flag is set based on all that up there! :D
    setFlag(V, (~((u16) a ^ (u16) fetched) & ((u16) a ^ (u16) temp)) & 0x0080);

    // The negative flag is set to the most significant bit of the result
    setFlag(N, temp & 0x80);

    // Load the result into the accumulator (it's 8-bit dont forget!)
    a = temp & 0x00FF;

    // This instruction has the potential to require an additional clock cycle
    return 1;
}

u8 olc6502::SBC() {
    fetch();

    // Operating in 16-bit domain to capture carry out

    // We can invert the bottom 8 bits with bitwise xor
    u16 value = ((u16) fetched) ^ 0x00FF;

    // Notice this is exactly the same as addition from here!
    temp = (u16) a + value + (u16) getFlag(C);
    setFlag(C, temp & 0xFF00);
    setFlag(Z, ((temp & 0x00FF) == 0));
    setFlag(V, (temp ^ (u16) a) & (temp ^ value) & 0x0080);
    setFlag(N, temp & 0x0080);
    a = temp & 0x00FF;
    return 1;
}


// Instruction: Bitwise Logic AND
// Function:    A = A & M
// Flags Out:   N, Z
u8 olc6502::AND() {
    fetch();
    a = a & fetched;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 0;
}

// Instruction: Arithmetic Shift Left
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
// dependeing on add mode, it will update either a or the memory.
u8 olc6502::ASL() {
    fetch();
    temp = (u16) fetched << 1;
    setFlag(C, temp & 0xff00);
    setFlag(Z, (temp | 0x00ff) & 0x00);
    setFlag(N, temp & 0x80);
    if (lookup[opcode].addrmode == &olc6502::IMP) {
        a = temp & 0x00ff;
    } else {
        write(addr_abs, temp & 0x00ff);
    }
    return 0;
}

// Instruction: Branch if Carry Clear
// Function:    if(C == 0) pc = address
u8 olc6502::BCC() {
    if (getFlag(C) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;
        if (addr_abs & 0xff00 != pc & 0xff00)
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

u8 olc6502::BCS() {
    if (getFlag(C)) {
        cycles++;
        addr_abs = pc + addr_rel;
        if (addr_abs & 0xff00 != pc & 0xff00)
            cycles++;
        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Equal
// Function:    if(Z == 1) pc = address
u8 olc6502::BEQ() {
    if (getFlag(Z) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

u8 olc6502::BIT() {
    fetch();
    temp = a & fetched;
    setFlag(Z, (temp & 0x00FF) == 0x00);
    setFlag(N, fetched & (1 << 7));
    setFlag(V, fetched & (1 << 6));
    return 0;
}


// Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
u8 olc6502::BMI() {
    if (getFlag(N) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}


// Instruction: Branch if Not Equal
// Function:    if(Z == 0) pc = address
u8 olc6502::BNE() {
    if (getFlag(Z) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}


// Instruction: Branch if Positive
// Function:    if(N == 0) pc = address
u8 olc6502::BPL() {
    if (getFlag(N) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Break
// Function:    Program Sourced Interrupt
u8 olc6502::BRK() {
    pc++;

    setFlag(I, 1);
    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    setFlag(B, 1);
    write(0x0100 + stkp, status);
    stkp--;
    setFlag(B, 0);

    pc = (u16) read(0xFFFE) | ((u16) read(0xFFFF) << 8);
    return 0;
}


// Instruction: Branch if Overflow Clear
// Function:    if(V == 0) pc = address
u8 olc6502::BVC() {
    if (getFlag(V) == 0) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}


// Instruction: Branch if Overflow Set
// Function:    if(V == 1) pc = address
u8 olc6502::BVS() {
    if (getFlag(V) == 1) {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}


// Instruction: Clear Carry Flag
// Function:    C = 0
u8 olc6502::CLC() {
    setFlag(C, false);
    return 0;
}


// Instruction: Clear Decimal Flag
// Function:    D = 0
u8 olc6502::CLD() {
    setFlag(D, false);
    return 0;
}


// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
u8 olc6502::CLI() {
    setFlag(I, false);
    return 0;
}


// Instruction: Clear Overflow Flag
// Function:    V = 0
u8 olc6502::CLV() {
    setFlag(V, false);
    return 0;
}

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
u8 olc6502::CMP() {
    fetch();
    temp = (u16) a - (u16) fetched;
    setFlag(C, a >= fetched);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    return 1;
}


// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
u8 olc6502::CPX() {
    fetch();
    temp = (u16) x - (u16) fetched;
    setFlag(C, x >= fetched);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    return 0;
}


// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
u8 olc6502::CPY() {
    fetch();
    temp = (u16) y - (u16) fetched;
    setFlag(C, y >= fetched);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    return 0;
}


// Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
u8 olc6502::DEC() {
    fetch();
    temp = fetched - 1;
    write(addr_abs, temp & 0x00FF);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    return 0;
}


// Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
u8 olc6502::DEX() {
    x--;
    setFlag(Z, x == 0x00);
    setFlag(N, x & 0x80);
    return 0;
}


// Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
u8 olc6502::DEY() {
    y--;
    setFlag(Z, y == 0x00);
    setFlag(N, y & 0x80);
    return 0;
}


// Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
u8 olc6502::EOR() {
    fetch();
    a = a ^ fetched;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 1;
}


// Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
u8 olc6502::INC() {
    fetch();
    temp = fetched + 1;
    write(addr_abs, temp & 0x00FF);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    return 0;
}


// Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
u8 olc6502::INX() {
    x++;
    setFlag(Z, x == 0x00);
    setFlag(N, x & 0x80);
    return 0;
}


// Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
u8 olc6502::INY() {
    y++;
    setFlag(Z, y == 0x00);
    setFlag(N, y & 0x80);
    return 0;
}


// Instruction: Jump To Location
// Function:    pc = address
u8 olc6502::JMP() {
    pc = addr_abs;
    return 0;
}


// Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc = address
u8 olc6502::JSR() {
    pc--;

    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    pc = addr_abs;
    return 0;
}


// Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
u8 olc6502::LDA() {
    fetch();
    a = fetched;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 1;
}


// Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
u8 olc6502::LDX() {
    fetch();
    x = fetched;
    setFlag(Z, x == 0x00);
    setFlag(N, x & 0x80);
    return 1;
}


// Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
u8 olc6502::LDY() {
    fetch();
    y = fetched;
    setFlag(Z, y == 0x00);
    setFlag(N, y & 0x80);
    return 1;
}

u8 olc6502::LSR() {
    fetch();
    setFlag(C, fetched & 0x0001);
    temp = fetched >> 1;
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 olc6502::NOP() {
    // Sadly not all NOPs are equal, Ive added a few here
    // based on https://wiki.nesdev.com/w/index.php/CPU_unofficial_opcodes
    // and will add more based on game compatibility, and ultimately
    // I'd like to cover all illegal opcodes too
    switch (opcode) {
        case 0x1C:
        case 0x3C:
        case 0x5C:
        case 0x7C:
        case 0xDC:
        case 0xFC:
            return 1;
            break;
    }
    return 0;
}


// Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
u8 olc6502::ORA() {
    fetch();
    a = a | fetched;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 1;
}


// Instruction: Push Accumulator to Stack
// Function:    A -> stack
u8 olc6502::PHA() {
    write(0x0100 + stkp, a);
    stkp--;
    return 0;
}


// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
u8 olc6502::PHP() {
    write(0x0100 + stkp, status | B | U);
    setFlag(B, 0);
    setFlag(U, 0);
    stkp--;
    return 0;
}


// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
u8 olc6502::PLA() {
    stkp++;
    a = read(0x0100 + stkp);
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 0;
}


// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
u8 olc6502::PLP() {
    stkp++;
    status = read(0x0100 + stkp);
    setFlag(U, 1);
    return 0;
}

u8 olc6502::ROL() {
    fetch();
    temp = (u16) (fetched << 1) | getFlag(C);
    setFlag(C, temp & 0xFF00);
    setFlag(Z, (temp & 0x00FF) == 0x0000);
    setFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 olc6502::ROR() {
    fetch();
    temp = (u16) (getFlag(C) << 7) | (fetched >> 1);
    setFlag(C, fetched & 0x01);
    setFlag(Z, (temp & 0x00FF) == 0x00);
    setFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &olc6502::IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

u8 olc6502::RTI() {
    stkp++;
    status = read(0x0100 + stkp);
    status &= ~B;
    status &= ~U;

    stkp++;
    pc = (u16) read(0x0100 + stkp);
    stkp++;
    pc |= (u16) read(0x0100 + stkp) << 8;
    return 0;
}

u8 olc6502::RTS() {
    stkp++;
    pc = (u16) read(0x0100 + stkp);
    stkp++;
    pc |= (u16) read(0x0100 + stkp) << 8;

    pc++;
    return 0;
}


// Instruction: Set Carry Flag
// Function:    C = 1
u8 olc6502::SEC() {
    setFlag(C, true);
    return 0;
}


// Instruction: Set Decimal Flag
// Function:    D = 1
u8 olc6502::SED() {
    setFlag(D, true);
    return 0;
}


// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
u8 olc6502::SEI() {
    setFlag(I, true);
    return 0;
}


// Instruction: Store Accumulator at Address
// Function:    M = A
u8 olc6502::STA() {
    write(addr_abs, a);
    return 0;
}


// Instruction: Store X Register at Address
// Function:    M = X
u8 olc6502::STX() {
    write(addr_abs, x);
    return 0;
}


// Instruction: Store Y Register at Address
// Function:    M = Y
u8 olc6502::STY() {
    write(addr_abs, y);
    return 0;
}


// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
u8 olc6502::TAX() {
    x = a;
    setFlag(Z, x == 0x00);
    setFlag(N, x & 0x80);
    return 0;
}


// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
u8 olc6502::TAY() {
    y = a;
    setFlag(Z, y == 0x00);
    setFlag(N, y & 0x80);
    return 0;
}


// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
u8 olc6502::TSX() {
    x = stkp;
    setFlag(Z, x == 0x00);
    setFlag(N, x & 0x80);
    return 0;
}


// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
u8 olc6502::TXA() {
    a = x;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 0;
}


// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
u8 olc6502::TXS() {
    stkp = x;
    return 0;
}


// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
u8 olc6502::TYA() {
    a = y;
    setFlag(Z, a == 0x00);
    setFlag(N, a & 0x80);
    return 0;
}


// This function captures illegal opcodes
u8 olc6502::XXX() {
    return 0;
}
