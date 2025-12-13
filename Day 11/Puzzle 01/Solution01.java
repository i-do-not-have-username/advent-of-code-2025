import java.io.*;
import java.math.BigInteger;
import java.util.*;

public class Solution01 {
    static final BigInteger ONE = BigInteger.ONE;
    static final BigInteger ZERO = BigInteger.ZERO;

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        Map<String, List<String>> graph = new HashMap<>();

        String line;
        while ((line = br.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) continue;

            // Format: "name: a b c" (outputs separated by spaces) :contentReference[oaicite:2]{index=2}
            String[] parts = line.split(":", 2);
            String from = parts[0].trim();

            List<String> outs = new ArrayList<>();
            if (parts.length == 2) {
                String rhs = parts[1].trim();
                if (!rhs.isEmpty()) {
                    for (String to : rhs.split("\\s+")) outs.add(to);
                }
            }
            graph.put(from, outs);
        }

        // Memoized DFS from "you" to "out" :contentReference[oaicite:3]{index=3}
        Map<String, BigInteger> memo = new HashMap<>();
        Map<String, Integer> state = new HashMap<>(); // 0/absent=unvisited, 1=visiting, 2=done

        BigInteger ans = dfs("you", graph, memo, state);
        System.out.println(ans);
    }

    private static BigInteger dfs(
            String node,
            Map<String, List<String>> graph,
            Map<String, BigInteger> memo,
            Map<String, Integer> state
    ) {
        if ("out".equals(node)) return ONE; // sink contributes one completed path :contentReference[oaicite:4]{index=4}
        if (memo.containsKey(node)) return memo.get(node);

        int st = state.getOrDefault(node, 0);
        if (st == 1) {
            // A cycle reachable from "you" would imply infinitely many distinct walks to "out"
            // (AoC inputs for this problem should avoid that).
            throw new IllegalStateException("Cycle detected involving: " + node);
        }
        state.put(node, 1);

        BigInteger sum = ZERO;
        for (String nxt : graph.getOrDefault(node, Collections.emptyList())) {
            sum = sum.add(dfs(nxt, graph, memo, state));
        }

        state.put(node, 2);
        memo.put(node, sum);
        return sum;
    }
}
