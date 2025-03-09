import java.util.concurrent.*;

public class Program1 {
    final static int NUM_WEBPAGES = 40;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    private static ForkJoinPool pool = ForkJoinPool.commonPool();
    private static BlockingQueue<WebPage> downloadQueue = new LinkedBlockingQueue<>(40);
    private static BlockingQueue<WebPage> analyzeQueue = new LinkedBlockingQueue<>(40);
    private static BlockingQueue<WebPage> categorizeQueue = new LinkedBlockingQueue<>(40);

    static class AnalyzeTask extends RecursiveAction {
        @Override
        protected void compute() {
            try {
                while (true) {
                    WebPage webpage = downloadQueue.take(); // Changed from poll() to take()
                    webpage.analyze();
                    analyzeQueue.put(webpage);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    static class CategorizeTask extends RecursiveAction {
        @Override
        protected void compute() {
            try {
                while (true) {
                    WebPage webpage = analyzeQueue.take(); // Changed from poll() to take()
                    webpage.categorize();
                    categorizeQueue.put(webpage);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
        }
    }

    private static void downloadWebPages() {
        for (WebPage page : webPages) {
            page.download();
            try {
                downloadQueue.put(page);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        try {
            downloadQueue.put(new WebPage(-1, "DONE")); // Poison pill added
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    private static void presentResult() {
        while (!categorizeQueue.isEmpty()) {
            System.out.println(categorizeQueue.poll());
        }
    }

    public static void main(String[] args) {
        initialize();

        long start = System.nanoTime();

        pool.submit(Program1::downloadWebPages);
        pool.submit(new AnalyzeTask());
        pool.submit(new CategorizeTask());

        pool.shutdown(); // Changed from awaitQuiescence() to shutdown()
        try {
            pool.awaitTermination(30, TimeUnit.SECONDS); // Proper termination
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        long stop = System.nanoTime();

        presentResult();

        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
