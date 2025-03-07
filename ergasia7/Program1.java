import java.util.concurrent.*;

public class Program1 {
    final static int NUM_WEBPAGES = 40;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    private static ForkJoinPool pool = ForkJoinPool.commonPool();
    private static BlockingQueue<WebPage> downloadQueue = new LinkedBlockingQueue<>();
    private static BlockingQueue<WebPage> analyzeQueue = new LinkedBlockingQueue<>();

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
    }

    private static void analyzeWebPages() {
        while (!downloadQueue.isEmpty()) {
            try {
                WebPage page = downloadQueue.take();
                page.analyze();
                analyzeQueue.put(page);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    private static void categorizeWebPages() {
        while (!analyzeQueue.isEmpty()) {
            try {
                WebPage page = analyzeQueue.take();
                page.categorize();
                System.out.println(page);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public static void main(String[] args) {
        initialize();

        long start = System.nanoTime();

        pool.submit(Program1::downloadWebPages);
        pool.submit(Program1::analyzeWebPages);
        pool.submit(Program1::categorizeWebPages);

        pool.shutdown();
        try {
            pool.awaitTermination(30, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        long stop = System.nanoTime();
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
