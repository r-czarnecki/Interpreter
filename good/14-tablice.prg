int main () {
    int[] t = new int[5];
    t[1] = 10;
    print t;
    print (size t);
    int[] t2 = t;
    resize t2 10;
    t[4] = -1;

    print t;
    print (size t);
    print t2;
    print (size t2);

    bool[] t3 = [false, true, true, false];
    print t3;
    print t3[2];
}