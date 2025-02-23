class RefExample {
    void incrementBy(ref int x, int y) {
        x = x + y;
    }

    void applyIncrements() {
        int[5] values;
        int base;
        int i;
        int fa2;
    
        print(values[0]);
        base = 0xf;
       
        fa2 = 5;
       
        i = 0;
        while (i < fa2) {
            incrementBy( base, i);
            values[i] = base + i;
            i = i + 1;
        }
        print(fa2);

     
    }

    void main() {
        applyIncrements();

        print(values[0]);
        print(values[1]);
        print(values[2]);
        print(values[3]);
        print(values[4]);
    }
}