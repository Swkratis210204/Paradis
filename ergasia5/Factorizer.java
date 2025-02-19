import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Scanner;

public class Factorizer {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        System.out.println("Give the product of two prime numbers:");
        BigInteger product = in.nextBigInteger();

        if (isPrime(product)) {
            System.out.println("No factorization possible");
            in.close();
            return;
        }

        System.out.println("Give the number of threads:");
        int numOfThreads = in.nextInt();

        long startTime = System.currentTimeMillis();

        Mythreads myThreads = new Mythreads(product, numOfThreads);
        myThreads.startThreads();

        long endTime = System.currentTimeMillis(); // End time measurement
        System.out.println("Execution Time: " + (endTime - startTime) + " ms");

        in.close();
    }

    static class Mythreads {
        private Thread[] threads;
        private int numOfThreads;
        private BigInteger product;

        Mythreads(BigInteger product, int numOfThreads) {
            this.threads = new Thread[numOfThreads];
            this.numOfThreads = numOfThreads;
            this.product = product;
        }

        public void startThreads() {
            BigInteger baseSize = this.product.divide(BigInteger.valueOf(numOfThreads));
            BigInteger remainder = this.product.remainder(BigInteger.valueOf(numOfThreads));

            BigInteger start = BigInteger.TWO;
            BigInteger maxLimit = product.subtract(BigInteger.ONE);

            for (int i = 0; i < numOfThreads; i++) {
                BigInteger end = start.add(baseSize).subtract(BigInteger.ONE);
                
                if (BigInteger.valueOf(i).compareTo(remainder) < 0) {
                    end = end.add(BigInteger.ONE);
                }

                if (end.compareTo(maxLimit) > 0) {
                    end = maxLimit;
                }

                final BigInteger threadStart = start;
                final BigInteger threadEnd = end;
                final int threadId = i;

                System.out.println("Thread " + threadId + " assigned range: " + threadStart + " to " + threadEnd);

                threads[i] = new Thread(() -> {
                    ArrayList<BigInteger> factors = findPrime(threadStart, threadEnd, this.product);
                    
                    if (!factors.isEmpty()) {
                        System.out.println("Thread " + threadId + " found factors: " + factors.get(0) + " and " + factors.get(1));
                        System.exit(0);
                    } else {
                        System.out.println("Thread " + threadId + " found no factors in its range.");
                    }
                });

                threads[i].start();
                start = end.add(BigInteger.ONE);
            }
        }
    }

    public static ArrayList<BigInteger> findPrime(BigInteger min, BigInteger max, BigInteger product) {
        BigInteger number = min;

        while (number.compareTo(max) <= 0) {
            if (product.remainder(number).equals(BigInteger.ZERO)) {
                BigInteger factor1 = number;
                BigInteger factor2 = product.divide(factor1);

                if (!factor1.equals(BigInteger.ONE) && !factor2.equals(BigInteger.ONE) &&
                    !factor1.equals(product) && !factor2.equals(product)) {
                    
                    System.out.println("Found valid factors: " + factor1 + " and " + factor2);

                    ArrayList<BigInteger> factors = new ArrayList<>();
                    factors.add(factor1);
                    factors.add(factor2);
                    
                    return factors;
                }
            }
            number = number.add(BigInteger.ONE);
        }

        System.out.println("No valid factors found in range: " + min + " to " + max);
        return new ArrayList<>();
    }

    public static boolean isPrime(BigInteger num) {
        if (num.compareTo(BigInteger.TWO) < 0) return false;
        if (num.equals(BigInteger.TWO)) return true;
        if (num.mod(BigInteger.TWO).equals(BigInteger.ZERO)) return false;
        
        BigInteger sqrt = num.sqrt().add(BigInteger.ONE);
        for (BigInteger i = BigInteger.valueOf(3); i.compareTo(sqrt) <= 0; i = i.add(BigInteger.TWO)) {
            if (num.mod(i).equals(BigInteger.ZERO)) return false;
        }
        return true;
    }
}
