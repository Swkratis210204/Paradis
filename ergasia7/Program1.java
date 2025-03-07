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
                    WebPage webpage = downloadQueue.poll(1, TimeUnit.SECONDS);
                    if (webpage == null) break; // Exit when no more work is available
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
                    WebPage webpage = analyzeQueue.poll(1, TimeUnit.SECONDS);
                    if (webpage == null) break; // Exit when no more work is available
                    webpage.categorize();
                    categorizeQueue.put(webpage);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    // Initialize method (not parallelized)
    private static void initialize() {
        for (int i = 0; i < NUM_WEBPAGES; i++) {
            webPages[i] = new WebPage(i, "http://www.site.se/page" + i + ".html");
        }
    }

    // Download method (Producer)
    private static void downloadWebPages() {
        for (WebPage page : webPages) {
            page.download();
            try {
                downloadQueue.put(page);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    // Present results (not parallelized)
    private static void presentResult() {
        while (!categorizeQueue.isEmpty()) {
            System.out.println(categorizeQueue.poll());
        }
    }

    public static void main(String[] args) {
        // Initialize the list of webpages.
        initialize();

        // Start timing.
        long start = System.nanoTime();

        // Submit all tasks in parallel
        pool.submit(Program1::downloadWebPages);
        pool.submit(new AnalyzeTask());
        pool.submit(new CategorizeTask());

        // Wait for all tasks to finish
        pool.awaitQuiescence(10, TimeUnit.SECONDS);

        // Stop timing.
        long stop = System.nanoTime();

        // Present the result.
        presentResult();

        // Present the execution time.
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
