#include <stdio.h>
#include <stdlib.h>

typedef struct
{
	int data;
	struct NODE *next;
} NODE;

void create_nodes(NODE *head, int);
void display_nodes(NODE *head);

int main(void)
{
	int n = 0;

	printf("Number of LinkedList Nodes : ");
	scanf("%d", &n);

	if (n < 0)
	{
		printf("Number of Nodes cannot be Negative");
		exit(1);
	}

	NODE *head = NULL;

	create_nodes(head, n);

	display_nodes(head);

	return 0;
}

void create_nodes(NODE *head, int n)
{
	for (int i = 0; i < n; i++)
	{
		NODE *nn = (NODE *)malloc(sizeof(NODE));
		if (nn == NULL)
		{
			print("MEM ALLOC Failed for NODE-%d", i+1);
			exit(1);
		}

		int dat = 0;
		printf("Enter Data for NODE-%d : ", i+1);
		scanf("%d", &dat);
		nn->data = dat;
		if (head == NULL)
		{
			head = nn;
		}
		else
		{
			NODE *temp = head;
			while (temp->next != NULL)
			{
				temp = temp->next;
			}
			temp->next = nn;
		}
	}
}
