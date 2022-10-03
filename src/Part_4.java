import java.util.Arrays;

public class Part_4 {
	public static int games=100000; // Equals to the number of simulation
	public static int goal=100; // Equals to the goal number
	public static int num=10; // Equals to the gap mentioned in strategy D
	public static int opponent=30; // Equals to when the player starts to use strategy D-10
	public static void main(String[] args) {
		int scoreA=0;
		int scoreB=0;
		int steps=0;
		int winA=0;
		int winB=0;
		float allA[]=new float[10];
		float allB[]=new float[10];
		// To check the convergence, do 100000 simulations for 10 times
		for (int j=0;j<10;j++) {
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
						// Strategy D
						scoreA+=rand;
						if (scoreA-scoreB<=num && scoreA%2==0 && scoreA<goal && scoreB>=opponent) {
							scoreA=gamble(scoreA);
						}
					} else {
						// Strategy A
						/*
						if (rand%2==0) {
							scoreB+=rand;
						}
						if (scoreB%2==0 && scoreB<goal) {
							scoreB=gamble(scoreB);
						}
						*/
						// Strategy B
						/*
						scoreB+=rand;
						if (scoreB<10 && scoreB%2==0 && scoreB<goal) {
							scoreB=gamble(scoreB);
						}
						*/
						// Strategy C
						/*
						scoreB+=rand;
						if (scoreA>scoreB && scoreB%2==0 && scoreB<goal) {
							scoreB=gamble(scoreB);
						}
						*/
						// Strategy E
						
						scoreB+=rand;
						
						// Strategy F
						/*
						scoreB+=rand;
						if (scoreB%2==0 && scoreB<goal) {
							scoreB=gamble(scoreB);
						}
						*/
					}
				}
				if (scoreA>=goal) {
					winA+=1; // Count the number of times player 1 wins
				} else {
					winB+=1; // Count the number of times player 2 wins
				}
			}
			// Calculate the average win rate of strategy of 100000 simulations
			allA[j]=(float) (winA*1.0/games);
			// Calculate the average win rate of strategy of 100000 simulations
			allB[j]=(float) (winB*1.0/games);
		}
		Arrays.sort(allA);
		System.out.print("["+allA[0]+", "+allA[9]+"]");
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