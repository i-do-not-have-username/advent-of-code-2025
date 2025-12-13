import java.util.*;

public class Solution02 {
    static List<int[]> redTiles;
    static Set<String> redTileSet;
    static Map<String, Boolean> validCache = new HashMap<>();
    
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        redTiles = new ArrayList<>();
        redTileSet = new HashSet<>();
        
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine().trim();
            if (line.isEmpty()) continue;
            
            String[] parts = line.split(",");
            int x = Integer.parseInt(parts[0]);
            int y = Integer.parseInt(parts[1]);
            redTiles.add(new int[]{x, y});
            redTileSet.add(x + "," + y);
        }
        scanner.close();
        
        long maxArea = 0;
        
        // Try all pairs of red tiles as opposite corners
        for (int i = 0; i < redTiles.size(); i++) {
            for (int j = i + 1; j < redTiles.size(); j++) {
                int[] tile1 = redTiles.get(i);
                int[] tile2 = redTiles.get(j);
                
                int x1 = Math.min(tile1[0], tile2[0]);
                int x2 = Math.max(tile1[0], tile2[0]);
                int y1 = Math.min(tile1[1], tile2[1]);
                int y2 = Math.max(tile1[1], tile2[1]);
                
                long width = (long)(x2 - x1 + 1);
                long height = (long)(y2 - y1 + 1);
                long area = width * height;
                
                // Skip if can't beat current max
                if (area <= maxArea) continue;
                
                // Check if rectangle is valid
                if (isRectangleValid(x1, y1, x2, y2)) {
                    maxArea = area;
                }
            }
        }
        
        System.out.println(maxArea);
    }
    
    static boolean isRectangleValid(int x1, int y1, int x2, int y2) {
        long totalTiles = (long)(x2 - x1 + 1) * (y2 - y1 + 1);
        
        // Check all four corners first
        if (!isValidTile(x1, y1) || !isValidTile(x2, y2) ||
            !isValidTile(x1, y2) || !isValidTile(x2, y1)) {
            return false;
        }
        
        // For rectangles up to 10 million tiles, check exhaustively
        if (totalTiles <= 10000000) {
            for (int x = x1; x <= x2; x++) {
                for (int y = y1; y <= y2; y++) {
                    if (!isValidTile(x, y)) {
                        return false;
                    }
                }
            }
            return true;
        }
        
        // For larger rectangles, sample more densely
        int sampleCount = 0;
        int step = (int)Math.max(1, Math.sqrt(totalTiles) / 100);
        
        for (int x = x1; x <= x2; x += step) {
            for (int y = y1; y <= y2; y += step) {
                if (!isValidTile(x, y)) {
                    return false;
                }
                sampleCount++;
            }
        }
        
        // Also check the edges more carefully
        for (int x = x1; x <= x2; x++) {
            if (!isValidTile(x, y1) || !isValidTile(x, y2)) {
                return false;
            }
        }
        for (int y = y1; y <= y2; y++) {
            if (!isValidTile(x1, y) || !isValidTile(x2, y)) {
                return false;
            }
        }
        
        return true;
    }
    
    static boolean isValidTile(int x, int y) {
        String key = x + "," + y;
        if (validCache.containsKey(key)) {
            return validCache.get(key);
        }
        
        boolean valid = isValidTileUncached(x, y);
        validCache.put(key, valid);
        return valid;
    }
    
    static boolean isValidTileUncached(int x, int y) {
        if (redTileSet.contains(x + "," + y)) {
            return true;
        }
        
        for (int i = 0; i < redTiles.size(); i++) {
            int[] p1 = redTiles.get(i);
            int[] p2 = redTiles.get((i + 1) % redTiles.size());
            if (isOnLineSegment(x, y, p1, p2)) {
                return true;
            }
        }
        
        return isInsidePolygon(x, y);
    }
    
    static boolean isOnLineSegment(int x, int y, int[] p1, int[] p2) {
        int x1 = p1[0], y1 = p1[1];
        int x2 = p2[0], y2 = p2[1];
        
        if (y1 == y2 && y == y1) {
            return x >= Math.min(x1, x2) && x <= Math.max(x1, x2);
        }
        
        if (x1 == x2 && x == x1) {
            return y >= Math.min(y1, y2) && y <= Math.max(y1, y2);
        }
        
        return false;
    }
    
    static boolean isInsidePolygon(int x, int y) {
        int n = redTiles.size();
        boolean inside = false;
        
        for (int i = 0, j = n - 1; i < n; j = i++) {
            int xi = redTiles.get(i)[0], yi = redTiles.get(i)[1];
            int xj = redTiles.get(j)[0], yj = redTiles.get(j)[1];
            
            boolean intersect = ((yi > y) != (yj > y)) &&
                               (x < (xj - xi) * (y - yi) / (double)(yj - yi) + xi);
            if (intersect) inside = !inside;
        }
        
        return inside;
    }
}
