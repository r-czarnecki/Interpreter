int main() {
    int a = 0;
    while (a < 10) {
        if (a < 3) 
            print (toString a) + ": a < 3";
        else if (a < 6)
            print (toString a) + ": 3 <= a < 6";
        else
            print (toString a) + ": 6 <= a";

        if (a == 5) print (toString a) + ": a == 5";

        a++;
    }
}