import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class Solution02 {
    private static final BigInteger ONE = BigInteger.ONE;
    private static final BigInteger ZERO = BigInteger.ZERO;

    // masks: 1 = visited dac, 2 = visited fft
    private static int addMask(String node, int mask) {
        if ("dac".equals(node)) mask |= 1;
        if ("fft".equals(node)) mask |= 2;
        return mask;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        Map<String, List<String>> g = new HashMap<>();

        String line;
        while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) continue;

            // Format: "name: a b c" (directed outputs) :contentReference[oaicite:2]{index=2}
            String[] parts = line.split(":", 2);
            String from = parts[0].trim();
            List<String> outs = new ArrayList<>();

            if (parts.length == 2) {
                String rhs = parts[1].trim();
                if (!rhs.isEmpty()) {
                    for (String to : rhs.split("\\s+")) outs.add(to);
                }
            }
            g.put(from, outs);
        }

        // memo[node][mask] for mask in 0..3
        Map<String, BigInteger[]> memo = new HashMap<>();
        Map<String, byte[]> state = new HashMap<>(); // 0=unvisited,1=visiting,2=done per mask

        BigInteger ans = dfs("svr", 0, g, memo, state);
        System.out.println(ans);
    }

    private static BigInteger dfs(
            String node,
            int mask,
            Map<String, List<String>> g,
            Map<String, BigInteger[]> memo,
            Map<String, byte[]> state
    ) {
        mask = addMask(node, mask);

        if ("out".equals(node)) {
            return (mask == 3) ? ONE : ZERO;
        }

        BigInteger[] memArr = memo.computeIfAbsent(node, k -> new BigInteger[4]);
        if (memArr[mask] != null) return memArr[mask];

        byte[] stArr = state.computeIfAbsent(node, k -> new byte[4]);
        if (stArr[mask] == 1) {
            // A cycle reachable from svr that can still reach out would imply infinitely many paths.
            // AoC inputs for this kind of path-counting problem are expected to avoid that. :contentReference[oaicite:3]{index=3}
            throw new IllegalStateException("Cycle detected at node=" + node + " mask=" + mask);
        }

        stArr[mask] = 1;

        BigInteger sum = ZERO;
        for (String nxt : g.getOrDefault(node, Collections.emptyList())) {
            sum = sum.add(dfs(nxt, mask, g, memo, state));
        }

        stArr[mask] = 2;
        memArr[mask] = sum;
        return sum;
    }
}
