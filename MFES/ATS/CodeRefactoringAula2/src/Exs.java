public class Exs {

    /* We will be refactoring this function automatically. */
    // Ex1
    public void func1(){
        int[] values = {1,3,5,7,9};
        //int result = Arrays.stream(values).map(v -> v*v).sum();

        // Manual
        int result1 = 0;
        for(int v1 : values){
            result1 += (v1*v1);
        }

        // Automatic
        int result = 0;
        for (int v : values) {
            int i = v * v;
            result += i;
        }
    }

    // Ex2


}
