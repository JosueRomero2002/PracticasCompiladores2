class ArrayMaxFinder {
    int findMax(int[5] arr) {
        int max, i;
        max = arr[0];
        i = 1;
        while (i < 5) {
            if (arr[i] > max) {
                max = arr[i];
            }
            i = i + 1;
        }
        return max;
    }

    int main() {
        int[5] arr;
        int maxim;
        arr[0] = 64;
        arr[1] = 34;
        arr[2] = 25;
        arr[3] = 12;
        arr[4] = 22;
        
        maxim = findMax(arr);
        print(maxim);
       
    }
}
