int main () {
    int i = 0;
    while (true) {
        i++;
        if (i == 5) {
            continue;
        }

        if (i == 10) {
            break;
        }
        print i;
    }
}
