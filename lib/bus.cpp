#include "bus.h"

Bus::Bus() {
    for (auto &i: ram) i = 0x00;

    cpu.connectBus(this);
}

u8 Bus::read(u16 add, bool ro) const {
    if (add >= 0x0000 && add <= 0xffff)
        return this->ram[add];
    return 0;
}

void Bus::write(u16 add, u8 v) {
    if (add >= 0x0000 && add <= 0xffff)
        this->ram[add] = v;
}
