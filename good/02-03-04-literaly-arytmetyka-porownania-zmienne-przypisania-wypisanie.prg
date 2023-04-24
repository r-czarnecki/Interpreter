int i = 4;
bool b = false;
string s = "str";

int main() {
    print "int tests:";
    print i + 9;
    print i == 5;
    print i == 4;
    print i >= 10;
    i = 7 * (3 + 2) - i;
    print i;
    print i < 20;
    print -i;

    print "bool tests:";
    print b == true;
    b = !b;
    print b;
    print (b || true) && (false || !false);

    print "str tests:";
    print s;
    print s == "str";
    print s != "not str";
    string s2 = " | new string";
    print s + s2;
}