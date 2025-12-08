import java.util.*;

public class Solution02 {
    static class Point {
        int x, y, z;
        
        Point(int x, int y, int z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }
    
    static class Edge implements Comparable<Edge> {
        int u, v;
        double distance;
        
        Edge(int u, int v, double distance) {
            this.u = u;
            this.v = v;
            this.distance = distance;
        }
        
        @Override
        public int compareTo(Edge other) {
            return Double.compare(this.distance, other.distance);
        }
    }
    
    static class UnionFind {
        int[] parent;
        int[] size;
        int numComponents;
        
        UnionFind(int n) {
            parent = new int[n];
            size = new int[n];
            numComponents = n;
            for (int i = 0; i < n; i++) {
                parent[i] = i;
                size[i] = 1;
            }
        }
        
        int find(int x) {
            if (parent[x] != x) {
                parent[x] = find(parent[x]);
            }
            return parent[x];
        }
        
        boolean union(int x, int y) {
            int rootX = find(x);
            int rootY = find(y);
            
            if (rootX == rootY) return false;
            
            if (size[rootX] < size[rootY]) {
                parent[rootX] = rootY;
                size[rootY] += size[rootX];
            } else {
                parent[rootY] = rootX;
                size[rootX] += size[rootY];
            }
            numComponents--;
            return true;
        }
        
        boolean isFullyConnected() {
            return numComponents == 1;
        }
    }
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        List<Point> points = new ArrayList<>();
        
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine().trim();
            if (line.isEmpty()) continue;
            
            String[] parts = line.split(",");
            int x = Integer.parseInt(parts[0]);
            int y = Integer.parseInt(parts[1]);
            int z = Integer.parseInt(parts[2]);
            points.add(new Point(x, y, z));
        }
        scanner.close();
        
        int n = points.size();
        
        // Calculate all pairwise distances
        List<Edge> edges = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                double dist = distance(points.get(i), points.get(j));
                edges.add(new Edge(i, j, dist));
            }
        }
        
        // Sort edges by distance
        Collections.sort(edges);
        
        // Connect until all in one circuit
        UnionFind uf = new UnionFind(n);
        Edge lastConnection = null;
        
        for (Edge edge : edges) {
            if (uf.union(edge.u, edge.v)) {
                lastConnection = edge;
                if (uf.isFullyConnected()) {
                    break;
                }
            }
        }
        
        // Multiply X coordinates of last connection
        if (lastConnection != null) {
            int x1 = points.get(lastConnection.u).x;
            int x2 = points.get(lastConnection.v).x;
            long result = (long) x1 * x2;
            System.out.println(result);
        }
    }
    
    static double distance(Point a, Point b) {
        long dx = a.x - b.x;
        long dy = a.y - b.y;
        long dz = a.z - b.z;
        return Math.sqrt(dx * dx + dy * dy + dz * dz);
    }
}
