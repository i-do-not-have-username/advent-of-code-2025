import java.util.Scanner;

public class Solution02 {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        
        int currentPosition = 50;
        int zeroCount = 0;
        
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine().trim();
            
            // Skip empty lines
            if (line.isEmpty()) {
                continue;
            }
            
            // Parse direction and distance
            char direction = line.charAt(0);
            int distance = Integer.parseInt(line.substring(1));
            
            // Count how many times we pass through 0 during this rotation
            if (direction == 'L') {
                zeroCount += countZeroCrossingsLeft(currentPosition, distance);
                currentPosition = (currentPosition - distance) % 100;
                if (currentPosition < 0) {
                    currentPosition += 100;
                }
            } else if (direction == 'R') {
                zeroCount += countZeroCrossingsRight(currentPosition, distance);
                currentPosition = (currentPosition + distance) % 100;
            }
        }
        
        scanner.close();
        System.out.println(zeroCount);
    }
    
    // Count how many times we pass through 0 when rotating LEFT
    private static int countZeroCrossingsLeft(int position, int distance) {
        // We visit positions: (position - 1), (position - 2), ..., (position - distance) mod 100
        // We pass through 0 when (position - i) % 100 == 0
        // This happens when position - i = 100k, i.e., i = position - 100k
        // We need 1 <= i <= distance
        
        int kMin = (int) Math.ceil((position - distance) / 100.0);
        int kMax = (int) Math.floor((position - 1) / 100.0);
        
        return Math.max(0, kMax - kMin + 1);
    }
    
    // Count how many times we pass through 0 when rotating RIGHT
    private static int countZeroCrossingsRight(int position, int distance) {
        // We visit positions: (position + 1), (position + 2), ..., (position + distance) mod 100
        // We pass through 0 when (position + i) % 100 == 0
        // This happens when position + i = 100k, i.e., i = 100k - position
        // We need 1 <= i <= distance
        
        int kMin = (int) Math.ceil((position + 1) / 100.0);
        int kMax = (int) Math.floor((position + distance) / 100.0);
        
        return Math.max(0, kMax - kMin + 1);
    }
}
