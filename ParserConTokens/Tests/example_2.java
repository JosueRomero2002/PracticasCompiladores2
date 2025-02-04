class FactorialCalculator {
    int factorial(int n) {
        if (n <= 1) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    int main() {
        int n;
        int resultado;
        n = 5;

        resultado = factorial(n);
        print(resultado);
    }
}
