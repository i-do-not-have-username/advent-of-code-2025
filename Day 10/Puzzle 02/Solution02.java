import java.io.*;
import java.math.BigInteger;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Solution02 {

    private static final BigInteger ZERO = BigInteger.ZERO;

    private static final Pattern PARENS = Pattern.compile("\\(([^)]*)\\)");
    private static final Pattern CURLY  = Pattern.compile("\\{([^}]*)\\}");

    static class Machine {
        List<int[]> buttons;
        int[] target;
        Machine(List<int[]> buttons, int[] target) {
            this.buttons = buttons;
            this.target = target;
        }
    }

    public static void main(String[] args) throws Exception {
        String input = readAll(System.in);
        long total = 0;

        for (String line : input.split("\\R")) {
            line = line.trim();
            if (line.isEmpty()) continue;
            Machine m = parseLine(line);
            total += solveMachine(m.buttons, m.target);
        }

        System.out.println(total);
    }

    private static String readAll(InputStream in) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        byte[] buf = new byte[1 << 16];
        int r;
        while ((r = in.read(buf)) != -1) bos.write(buf, 0, r);
        return bos.toString("UTF-8");
    }

    /** Parses one input line: ignore [lights], keep (buttons) and {targets}. */
    private static Machine parseLine(String line) {
        // targets
        Matcher mc = CURLY.matcher(line);
        if (!mc.find()) throw new IllegalArgumentException("No {..} target: " + line);
        int[] target = parseIntList(mc.group(1));

        // buttons
        List<int[]> buttons = new ArrayList<>();
        Matcher mp = PARENS.matcher(line);
        while (mp.find()) {
            String inside = mp.group(1).trim();
            if (inside.isEmpty()) {
                buttons.add(new int[0]);
            } else {
                buttons.add(parseIntList(inside));
            }
        }
        if (buttons.isEmpty()) throw new IllegalArgumentException("No buttons: " + line);

        return new Machine(buttons, target);
    }

    private static int[] parseIntList(String csv) {
        String[] parts = csv.split(",");
        int[] out = new int[parts.length];
        for (int i = 0; i < parts.length; i++) out[i] = Integer.parseInt(parts[i].trim());
        return out;
    }

    /**
     * Solve one machine:
     * - Build augmented matrix A|b with A in {0,1}
     * - Integer Gaussian elimination (fraction-free)
     * - Enumerate free variables with safe upper bounds, back-substitute pivots
     * - Return minimal sum of presses
     */
    private static long solveMachine(List<int[]> buttons, int[] target) {
        final int n = target.length;          // counters / equations
        final int m = buttons.size();         // variables / buttons

        boolean[][] affects = new boolean[n][m];
        for (int j = 0; j < m; j++) {
            for (int idx : buttons.get(j)) {
                if (idx < 0 || idx >= n) {
                    throw new IllegalArgumentException("Button index out of range: " + idx + " for n=" + n);
                }
                affects[idx][j] = true;
            }
        }

        // Upper bound for each x_j: x_j <= min target[i] over counters it affects (since all contributions are +1).
        int[] ub = new int[m];
        for (int j = 0; j < m; j++) {
            int min = Integer.MAX_VALUE;
            boolean any = false;
            for (int i = 0; i < n; i++) {
                if (affects[i][j]) {
                    any = true;
                    min = Math.min(min, target[i]);
                }
            }
            ub[j] = any ? min : 0; // a button that affects nothing is useless
        }

        // Build augmented matrix (n rows) x (m cols + rhs)
        BigInteger[][] mat = new BigInteger[n][m + 1];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) mat[i][j] = affects[i][j] ? BigInteger.ONE : ZERO;
            mat[i][m] = BigInteger.valueOf(target[i]);
        }

        // Eliminate
        ElimResult er = eliminateInteger(mat, n, m);
        int rank = er.rank;
        int[] pivotCols = er.pivotCols;

        // Check for inconsistency: 0 = nonzero
        for (int i = rank; i < n; i++) {
            boolean allZero = true;
            for (int j = 0; j < m; j++) {
                if (!mat[i][j].equals(ZERO)) { allZero = false; break; }
            }
            if (allZero && !mat[i][m].equals(ZERO)) {
                throw new IllegalStateException("No solution for a machine (should not happen with AoC input).");
            }
        }

        boolean[] isPivot = new boolean[m];
        for (int c : pivotCols) isPivot[c] = true;

        List<Integer> freeColsList = new ArrayList<>();
        for (int j = 0; j < m; j++) if (!isPivot[j]) freeColsList.add(j);

        // Sort free variables by small upper bound to shrink search early.
        freeColsList.sort(Comparator.comparingInt(j -> ub[j]));

        int freeCount = freeColsList.size();
        int[] freeCols = new int[freeCount];
        for (int i = 0; i < freeCount; i++) freeCols[i] = freeColsList.get(i);

        BigInteger[] x = new BigInteger[m];
        long[] best = { Long.MAX_VALUE };

        dfsFreeVars(0, freeCols, ub, x, 0L, mat, rank, pivotCols, best);

        return best[0];
    }

    static class ElimResult {
        int rank;
        int[] pivotCols;
        ElimResult(int rank, int[] pivotCols) { this.rank = rank; this.pivotCols = pivotCols; }
    }

    /**
     * Fraction-free / integer Gaussian elimination.
     * Row ops: Rr = Rr*(pivot/g) - Rp*(entry/g) to zero out the current column.
     * This preserves integer solutions and avoids rationals. :contentReference[oaicite:1]{index=1}
     */
    private static ElimResult eliminateInteger(BigInteger[][] a, int nRows, int nVars) {
        int rank = 0;
        List<Integer> pivots = new ArrayList<>();

        for (int col = 0; col < nVars && rank < nRows; col++) {
            int sel = -1;
            for (int r = rank; r < nRows; r++) {
                if (!a[r][col].equals(ZERO)) { sel = r; break; }
            }
            if (sel == -1) continue;

            // swap into pivot row
            if (sel != rank) {
                BigInteger[] tmp = a[sel];
                a[sel] = a[rank];
                a[rank] = tmp;
            }

            pivots.add(col);
            BigInteger pivot = a[rank][col];

            // eliminate below
            for (int r = rank + 1; r < nRows; r++) {
                BigInteger entry = a[r][col];
                if (entry.equals(ZERO)) continue;

                BigInteger g = pivot.abs().gcd(entry.abs());
                BigInteger mulRow = pivot.divide(g);   // pivot/g
                BigInteger mulPiv = entry.divide(g);   // entry/g

                // Rr = Rr*mulRow - Rp*mulPiv  (from col to rhs)
                for (int j = col; j <= nVars; j++) {
                    a[r][j] = a[r][j].multiply(mulRow).subtract(a[rank][j].multiply(mulPiv));
                }
            }

            rank++;
        }

        int[] pivotCols = pivots.stream().mapToInt(i -> i).toArray();
        return new ElimResult(rank, pivotCols);
    }

    private static void dfsFreeVars(
            int idx,
            int[] freeCols,
            int[] ub,
            BigInteger[] x,
            long sumSoFar,
            BigInteger[][] mat,
            int rank,
            int[] pivotCols,
            long[] best
    ) {
        if (sumSoFar >= best[0]) return;

        if (idx == freeCols.length) {
            // back-substitute to fill pivot variables, check integrality/nonnegativity
            BigInteger[] sol = Arrays.copyOf(x, x.length);
            for (int j = 0; j < sol.length; j++) if (sol[j] == null) sol[j] = ZERO;

            for (int r = rank - 1; r >= 0; r--) {
                int col = pivotCols[r];
                BigInteger rhs = mat[r][sol.length]; // last column is RHS

                for (int j = col + 1; j < sol.length; j++) {
                    BigInteger coeff = mat[r][j];
                    if (!coeff.equals(ZERO)) {
                        rhs = rhs.subtract(coeff.multiply(sol[j]));
                    }
                }

                BigInteger coeffPivot = mat[r][col];
                if (coeffPivot.equals(ZERO)) {
                    if (!rhs.equals(ZERO)) return; // inconsistent
                    sol[col] = ZERO;
                } else {
                    BigInteger[] divRem = rhs.divideAndRemainder(coeffPivot);
                    if (!divRem[1].equals(ZERO)) return;      // not integer
                    if (divRem[0].signum() < 0) return;       // negative presses
                    sol[col] = divRem[0];
                }
            }

            // compute total presses and update best
            long total = 0L;
            for (BigInteger v : sol) {
                // AoC targets are small enough for long; fail-fast if not.
                long lv;
                try {
                    lv = v.longValueExact();
                } catch (ArithmeticException ex) {
                    return;
                }
                total += lv;
                if (total >= best[0]) return;
            }
            best[0] = total;
            return;
        }

        int col = freeCols[idx];
        int upper = ub[col];

        for (int v = 0; v <= upper; v++) {
            long newSum = sumSoFar + v;
            if (newSum >= best[0]) break; // v only increases, so we can stop
            x[col] = BigInteger.valueOf(v);
            dfsFreeVars(idx + 1, freeCols, ub, x, newSum, mat, rank, pivotCols, best);
        }
        x[col] = null;
    }
}
