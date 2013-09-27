module task8;

import std.stdio;
import std.algorithm;

bool any(alias fun, R)(R haystack) {
    foreach(i; map!fun(haystack)) {
        if(i)
            return true;
    }
    return false;
}

bool all(alias fun, R)(R haystack) {
    foreach(i; map!fun(haystack)) {
        if(!i)
            return false;
    }
    return true;
}

unittest {
    auto a = [1, 2, 3, 4, 5];
    assert(a.any!("a<5")());
    assert(!a.any!("a>10")());
    assert(a.all!("a<10")());
    assert(!a.all!("a>3")());
}

bool atLeasOneEven(T)(T box) {
    return box.all!(row => row.any!("a % 2 ==0"))();
}

unittest {
    auto a = [[1,2,3],
              [4,5,6],
              [7,8,9]];
    assert(atLeasOneEven(a));
    auto b = [[1,2,3],
              [4,5,6],
              [7,7,9]];
    assert(!atLeasOneEven(b));
    int[][] c = [];
    int[][] d = [[]];
    assert(atLeasOneEven(c));
    assert(!atLeasOneEven(d));
}
