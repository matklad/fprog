module integrator;
import std.stdio;
import std.math;

real integral(real delegate(real) fun, real a, real b, int n=100)
{
    real h = (b - a) / n;
    real s = 0;
    foreach(i; 0..n-1) {
        s += fun(a + h / 2 + h*i) * h;
    }
    return s;
}

unittest {
    auto num = integral(x => x, 0, 2);
    assert(abs(num - 2) < 0.1);

    //argh! why I can't just use literal here?
    real delegate(real) f = x => sin(x);
    num = integral(f, 0, 2*PI);
    assert(abs(num) < 0.1);

    num = integral(x => x^^2, 0, 1);
    assert(abs(num - 0.33) < 0.1);
}

void main() {
    //no op
}
