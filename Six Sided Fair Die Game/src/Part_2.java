import java.util.Arrays;

public class Part_2 {
	public static int games=1000; // Equals to the number of simulation
	public static int goal=100; // Equals to the goal number
	// Strategy D and F
	public static void main(String[] args) {
		int scoreA=0;
		int scoreB=0;
		int steps=0;
		int winA=0;
		int winB=0;
		float allA[]=new float[games];
		float allB[]=new float[games];
		// To find out the distribution, do 1000 simulations for 1000 times
		for (int j=0;j<games;j++) {
			winA=0;
			winB=0;
			for (int i=0;i<games;i++) {
				scoreA=0;
				scoreB=0;
				steps=0;
				while (scoreA<goal && scoreB<goal) {
					int rand=(int)(1+Math.random()*6);
					steps+=1;
					if (steps%2==i%2) { // Let player 1 and player 2 take turns to start first
						// Let player 1 use strategy D
						scoreA+=rand;
						if (scoreA-scoreB<=6 && scoreA%2==0 && scoreA<goal) {
							scoreA=gamble(scoreA);
						}
					} else {
						// Let player 2 use strategy F
						scoreB+=rand;
						if (scoreB%2==0 && scoreB<goal) {
							scoreB=gamble(scoreB);
						}
					}
				}
				if (scoreA>=goal) {
					winA+=1; // Count the number of times player 1 wins
				} else {
					winB+=1; // Count the number of times player 2 wins
				}
			}
			// Calculate the average win rate of strategy D of 1000 simulations
			allA[j]=(float) (winA*1.0/games);
			// Calculate the average win rate of strategy F of 1000 simulations
			allB[j]=(float) (winB*1.0/games);
		}
		System.out.println(Arrays.toString(allA));
	}
	// The function for gamble
	public static int gamble(int score) {
		int rand=(int)(1+Math.random()*6);
		if (rand>3) {
			score+=10;
		} else {
			if (score-10<0) {
				score=0;
			} else {
				score-=10;
			}
		}
		return score;
	}
}