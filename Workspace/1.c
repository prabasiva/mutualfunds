#include <stdio.h>
#include <string.h>
int main()
{

FILE *fp_house;
FILE *fp_fund;
FILE *out;
char str[256];
char fund[256];

fp_house = fopen("Housing.txt","r");
if (fp_house == NULL)
{
	printf("Housing.txt file can't be open..\n");
	return 0;
}
fp_fund = fopen("NWUAX.csv","r");
if (fp_fund == NULL)
{
	printf("Fund file can't be open..\n");
	return 0;
}

while(fgets(fund,256,fp_fund)!=NULL)
{
printf("%s",fund);
while(fgets(str,256,fp_house)!=NULL)
{

printf("%s",str);
printf("%s",fund);

}
}

out = fopen("out.csv","w");
if (out == NULL)
{
	printf("Output file can't be open..\n");
	return 0;
}

}

