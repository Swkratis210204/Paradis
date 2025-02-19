// Author: Sokratis Vissarion Giannoutsos

import java.math.BigInteger;
import java.util.Scanner;

public class Factorizer {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        System.out.println("Give the product of two prime numbers:");
        BigInteger product = scanner.nextBigInteger();

        if (isPrime(product)) {
            System.out.println("No factorization possible");
            scanner.close();
            return;
        }

        System.out.println("Give the number of threads:");
        int numOfThreads = scanner.nextInt();
        scanner.close();

        int effectiveThreads = Math.min(numOfThreads, product.sqrt().intValue()); // Avoid excessive threads
        long startTime = System.currentTimeMillis();

        FactorizationThreads factorizationThreads = new FactorizationThreads(product, effectiveThreads);
        factorizationThreads.startThreads();

        long endTime = System.currentTimeMillis();
        System.out.println("Execution Time: " + (endTime - startTime) + " ms");
    }

    public static BigInteger[] findPrime(BigInteger min, BigInteger product, BigInteger step) {
        BigInteger number = min;

        while (number.compareTo(product) < 0) {
            if (Thread.currentThread().isInterrupted()) return null;

            if (product.remainder(number).equals(BigInteger.ZERO)) {
                BigInteger factor1 = number;
                BigInteger factor2 = product.divide(factor1);

                if (!factor1.equals(BigInteger.ONE) && !factor2.equals(BigInteger.ONE) &&
                        !factor1.equals(product) && !factor2.equals(product)) {
                    return new BigInteger[]{factor1, factor2};
                }
            }
            number = number.add(step);
        }
        return null;
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

class FactorizationThreads {
    private final Thread[] threads;
    private final int numOfThreads;
    private final BigInteger product;
    private volatile boolean found;

    public FactorizationThreads(BigInteger product, int numOfThreads) {
        this.threads = new Thread[numOfThreads];
        this.numOfThreads = numOfThreads;
        this.product = product;
        this.found = false;
    }

    public void startThreads() {
        for (int i = 0; i < numOfThreads; i++) {
            if (found) break; 

            final BigInteger threadStart = BigInteger.valueOf(i + 2);
            final int threadId = i;
            final BigInteger step = BigInteger.valueOf(numOfThreads);

            threads[i] = new Thread(() -> {
                if (!found) {
                    BigInteger[] factors = Factorizer.findPrime(threadStart, product, step);
                    if (factors != null) {
                        synchronized (this) {
                            if (!found) {
                                found = true;
                                System.out.println("Thread " + threadId + " found factors: " + factors[0] + " and " + factors[1]);
                                stopAllThreads();
                            }
                        }
                    }
                }
            });

            threads[i].start();
        }

        for (Thread thread : threads) {
            if (thread != null && thread.isAlive()) {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }
    }

    private void stopAllThreads() {
        for (Thread thread : threads) {
            if (thread != null) {
                thread.interrupt();
            }
        }
    }
}
