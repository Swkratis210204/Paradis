import java.util.concurrent.*;

public class Program3 {
    final static int NUM_WEBPAGES = 40;
    private static WebPage[] webPages = new WebPage[NUM_WEBPAGES];
    private static MyExecutor pool = new MyExecutor(3);
    private static BlockingQueue<WebPage> downloadQueue = new LinkedBlockingQueue<>(40);
    private static BlockingQueue<WebPage> analyzeQueue = new LinkedBlockingQueue<>(40);
    private static BlockingQueue<WebPage> categorizeQueue = new LinkedBlockingQueue<>(40);

    private static volatile int categorizedCount = 0; // 🟢 Counter to track processed webpages

    static class AnalyzeTask implements Runnable {
        @Override
        public void run() {
            try {
                while (categorizedCount < NUM_WEBPAGES) { // 🟢 Stop when all pages are categorized
                    WebPage webpage = downloadQueue.poll(1, TimeUnit.SECONDS);
                    if (webpage == null) continue; // 🟢 Wait for more tasks
                    webpage.analyze();
                    analyzeQueue.put(webpage);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    static class CategorizeTask implements Runnable {
        @Override
        public void run() {
            try {
                while (categorizedCount < NUM_WEBPAGES) { // 🟢 Stop when all pages are categorized
                    WebPage webpage = analyzeQueue.poll(1, TimeUnit.SECONDS);
                    if (webpage == null) continue; // 🟢 Wait for more tasks
                    webpage.categorize();
                    categorizeQueue.put(webpage);
                    synchronized (Program3.class) {
                        categorizedCount++; // 🟢 Track completion
                    }
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
    }

    private static void presentResult() {
        while (!categorizeQueue.isEmpty()) {
            System.out.println(categorizeQueue.poll());
        }
    }

    public static void main(String[] args) {
        initialize();

        long start = System.nanoTime();

        pool.execute(Program3::downloadWebPages);
        pool.execute(new AnalyzeTask());
        pool.execute(new CategorizeTask());

        // 🟢 Wait until all pages are categorized
        while (categorizedCount < NUM_WEBPAGES) {
            try {
                Thread.sleep(50); // Small delay to avoid busy waiting
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        pool.shutdown();
        try {
            pool.awaitTermination(5, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        presentResult();

        long stop = System.nanoTime();
        System.out.println("Execution time (seconds): " + (stop - start) / 1.0E9);
    }
}
