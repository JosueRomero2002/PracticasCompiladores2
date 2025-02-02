class NumberAnalyzer {
    void analyze(int x, int y) {
        if (x > 0 && y > 0) {
            print("Both numbers are positive");

        } else {
            print("Both numbers are zero or one of them is zero");
        }
    }

    int main() {
        int x;
        int y;
        x = -5;
        y = 10;
        analyze(x, y);
        return 0;
    }
}
