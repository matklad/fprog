module main11;

import task8; //use any from 8

auto CheckDigitCurried(int i){
    return ((int j) => j == i);
}

unittest {
    assert(CheckDigitCurried(2)(2));
    assert(!CheckDigitCurried(1)(2));
    auto a = [1, 2, 3];
    //without f compiler tries to compile time evaluate this and
    //Error: closures are not yet supported in CTFE
    //TT
    auto f = CheckDigitCurried(2);
    assert(a.any!(f));
    f = CheckDigitCurried(7);
    assert(!a.any!(f));
}

void main() {
}
