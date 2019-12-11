

// polygon(
//    points=[[0,0],[100,0],[0,100],[10,10],[80,10],[10,80]], 
//    paths=[[0,1,2],[3,4,5]]);

#include <stdio.h>

#define CAPACITY 65536

typedef struct point_tag {
	int x;
	int y;
} point;

int str2int(const char* s)
{
	int r = 0;
	int i = 0;

	while(s[i] >= '0' && s[i] <='9')
		r = 10*r + s[i++] - '0';

	return r;
}

void set_a(point* a, int x1, int y1, int upper, int off)
{
	for (int i=0; i<upper; i++) {
		if (i%2) {
			a[2*i+1].x = off+x1*i;
			a[2*i+1].y = y1;
			a[2*i+2].x = off+x1*i;
			a[2*i+2].y = 0;		
		} else {
			a[2*i+1].x = off+x1*i;
			a[2*i+1].y = 0;
			a[2*i+2].x = off+x1*i;
			a[2*i+2].y = y1;
		}
	}	
}

void set_b(point* b, int x, int x2, int y2, int upper, int off)
{
	for (int i=0; i<upper; i++) {
		if (i%2) {
			b[2*i+1].y = off+y2*i;
			b[2*i+1].x = x;
			b[2*i+2].y = off+y2*(i+1);
			b[2*i+2].x = x;		
		} else {
			b[2*i+1].y = off+y2*i;
			b[2*i+1].x = x-x2;
			b[2*i+2].y = off+y2*(i+1);
			b[2*i+2].x = x-x2;
		}
	}	
}

void set_c(point* c, point* a, int imax_x, int y)
{
	for (int i=0; i<=imax_x; i++) {
		c[imax_x-i].x = a[i].x;
		c[imax_x-i].y = y - a[i].y;
	}
}

void set_d(point* d, point* b, int imax_y, int x)
{
	for (int i=0; i<=imax_y; i++) {
		d[imax_y-i].x = x - b[i].x;
		d[imax_y-i].y = b[i].y;
	}
}

int main(int argc, char const *argv[])
{
	point a[CAPACITY];
	point b[CAPACITY];
	point c[CAPACITY];
	point d[CAPACITY];

	int x, y, x1, y1, x2, y2;
	int m1, m2, off;
	int imax_x, imax_y;
	int mode;

	if (argc != 8) {
		printf("%s %s\n", argv[0], "[1|2] x y x1 y1 x2 y2");
		return -1;
	}

	mode = str2int(argv[1]);
	if (mode != 1 && mode != 2) {
		printf("%s %s\n", argv[0], "[1|2] x y x1 y1 x2 y2");
		return -1;		
	}

	x  = str2int(argv[2]);
	y  = str2int(argv[3]);
	x1 = str2int(argv[4]);
	y1 = str2int(argv[5]);
	x2 = str2int(argv[6]);
	y2 = str2int(argv[7]);

	if (y1 > x1/2) {
		printf("y1 > x1/2, x1 %d, y1 %d\n", x1, y1);
		return -1;
	}

	if (x2 > y2/2) {
		printf("x2 > y2/2, x2 %d, y2 %d\n", x2, y2);
		return -1;
	}

	m1 = x / x1;
	if (m1 < 2) {
		printf("x / x1 < 2, x %d, x1 %d\n", x, x1);
		return -1;
	}
	if (m1 > CAPACITY/2) {
   		printf("m1 > CAPACITY/2, m1 %d, CAPACITY %d\n", m1, CAPACITY);
		return -1;
	}

	m2 = y / y2;
	if (m1 < 2) {
		printf("y / y2 < 2, y %d, y2 %d\n", x, x1);
		return -1;
	}
	if (m2 > CAPACITY/2) {
   		printf("m2 > CAPACITY/2, m2 %d, CAPACITY %d\n", m2, CAPACITY);
		return -1;
	}

	if (mode == 1) {
		a[0].x = 0;
		a[0].y = 0;
		if (m1%2) {
			off = x1+(x%x1)/2;
			set_a(a, x1, y1, m1-1, off);
			imax_x = 2*m1-1;		 
		} else {
			off = ((x%x1)+x1)/2;
			set_a(a, x1, y1, m1, off);
			imax_x = 2*m1+1;		
		}
		a[imax_x].x = x;
		a[imax_x].y = 0;	
		set_c(c, a, imax_x, y);

		if (m2%2) {
			off = y2+(y%y2)/2;
			set_b(b, x, x2, y2, m2-1, off);
			imax_y = 2*m2-2;		 
		} else {
			off = ((y%y2)+y2)/2;
			set_b(b, x, x2, y2, m2, off);
			imax_y = 2*m2;		
		}
		b[0].x = x;
		b[0].y = off;
		set_d(d, b, imax_y-1, x);
		d[imax_y].x = 0;
		d[imax_y].y = 0;		
	} 

	if (mode == 2) {
		a[0].x = 0;
		a[0].y = 0;
		if (m1%2) {
			off = x1+(x%x1)/2;
			set_a(a, x1, y1, m1-1, off);
			imax_x = 2*m1-1;		 
		} else {
			off = ((x%x1)+x1)/2;
			set_a(a, x1, y1, m1, off);
			imax_x = 2*m1+1;		
		}
		a[imax_x].x = x;
		a[imax_x].y = 0;	
		set_c(c, a, imax_x, y);

		for (int i=0; i<=imax_x; i++) {
			a[i].y = a[i].y == 0 ? y1   : 0;
			c[i].y = c[i].y == y ? y-y1 : y;
		}

		if (m2%2) {
			off = y2+(y%y2)/2;
			set_b(b, x, x2, y2, m2-1, off);
			imax_y = 2*m2-2;		 
		} else {
			off = ((y%y2)+y2)/2;
			set_b(b, x, x2, y2, m2, off);
			imax_y = 2*m2;		
		}
		b[0].x = x;
		b[0].y = off;
		set_d(d, b, imax_y-1, x);
		d[imax_y].x = 0;
		d[imax_y].y = 0;		

		for (int i=0; i<=imax_y; i++) {
			b[i].x = b[i].x == x ? x-y1 : x;
			d[i].x = d[i].x == 0 ? y1   : 0;
		}
	} 	

	printf("[[%d,%d], ", a[0].x, a[0].y);
	for (int i=1; i<=imax_x; i++) {
		printf("[%d,%d], ", a[i].x, a[i].y);
		if (!(i%8)) printf("\n ");
	}

	printf("\n [%d,%d], ", b[0].x, b[0].y);
	for (int i=1; i<=imax_y; i++) {
		printf("[%d,%d], ", b[i].x, b[i].y);
		if (!(i%8)) printf("\n ");
	}

	printf("\n [%d,%d], ", c[0].x, c[0].y);
	for (int i=1; i<=imax_x; i++) {
		printf("[%d,%d], ", c[i].x, c[i].y);
		if (!(i%8)) printf("\n ");
	}

	printf("[%d,%d], ", d[0].x, d[0].y);
	for (int i=1; i<imax_y; i++) {
		printf("[%d,%d], ", d[i].x, d[i].y);
		if (!(i%8)) printf("\n ");
	}	
	printf("[%d,%d]] ", d[imax_y].x, d[imax_y].y);

	printf("\n");

//	for (int i=0; i<m1+2; i++)
//		printf("(%d,%d) (%d,%d)\n", a[2*i].x, a[2*i].y, a[2*i+1].x, a[2*i+1].y);

	return 0;
}