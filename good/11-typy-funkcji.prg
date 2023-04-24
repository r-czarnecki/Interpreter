int i(int n) {
    return n;
}

string s(string n) {
    return n;
}

bool b(bool n) {
    return n;
}

string[] list(int[] n) {
    int arrSize = size n;
    string[] result = new string[arrSize];
    int i = 0;
    while (i < arrSize) {
        result[i] = toString n[i];
        i++;
    }

    return result;
}

Tuple(int, string, bool[], Tuple(string, bool)) tuple(Tuple(int, string, bool[]) t1, Tuple(string, bool) t2) {
    return (t1[0], t1[1], t1[2], (t2[0], t2[1]));
}

int main() {
    print i(10);
    print s("str");
    print b(true);
    print list([1, 3, 5, 2, -4]);
    print tuple((-3, "s", [false, true]), ("str2", false));
}