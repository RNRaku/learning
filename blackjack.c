#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int value(){
    int prob = rand()%13;
    if (prob < 4){
        return 10;
    } 
    else{
        return rand() % 9 + 1;
    }
}


int main (){
    int choice=0,total=0,turn=0,playertotal=0,dealertotal=0;
    printf("welcome to blackjack \n");

    
    srand(time(NULL));
    rand();

    int card1= value();
    int card2= value();

    total=card1+card2;

    printf("first card :%d  \n",card1);
    printf("first card :%d   \n",card2);
    printf("current total is :%d \n",total);


    while(choice!=-1){
        printf("1: hit \n");
        printf("2: stand \n");
        scanf("%d",&choice);

        if (choice==1){
            int cardvalue = value();
            srand(time(NULL));
            rand();
      
            printf("generaterd card is %d \n",cardvalue);
            
            if (cardvalue==1){
                int ace;
                printf("you got an ace, would you like it to count as a 1 or a 11 \n");
                scanf("%d",&ace);
                cardvalue=ace;
            }
            total+=cardvalue;
            printf("current hand value = %d \n",total);

        
            if(total>21){
                printf("hand value over 21, BUST \n");
                break;
            }
        }
        if (choice == 2){
            
            playertotal=total;
            printf("final hand value %d",playertotal);
            break;
        }

    }
}