$fn=24;

module rsquare (lx,ly,b) {
    union () {
        difference () {
            square ([lx,ly]);
            polygon (points=[[0,0],[0,b],[b,0]]);
            polygon (points=[[0,ly],[0,ly-b],[b,ly]]);
            polygon (points=[[lx,0],[lx,b],[lx-b,0]]);
            polygon (points=[[lx,ly],[lx,ly-b],[lx-b,ly]]);    
            }
        translate ([b,b,0]) { circle (b); } 
        translate ([b,ly-b,0]) { circle (b); } 
        translate ([lx-b,b,0]) { circle (b); } 
        translate ([lx-b,ly-b,0]) { circle (b); }
        }
}
b=12;
lx=376;
ly=250;

dx=24.5;
dy=18.6;
keyb_dx=10;
keyb_dy=7;

module inner_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy) {
    translate([keyb_dx,keyb_dy,0]) {
        difference () {
            rsquare(lx,ly,b);
            translate([dx,ly-dy,0]) { square([lx-2*dx,dy]); }
        }
    }
}

//inner_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy) ;

d_keyb=2;

module inner_keyb2 (lx,ly,b,dx,dy,keyb_dx,keyb_dy,d_keyb) {
    translate([keyb_dx+d_keyb,keyb_dy+d_keyb,0]) {
        difference () {
            rsquare(lx-2*d_keyb,ly-2*d_keyb,b-2*d_keyb);
            translate([dx-2*d_keyb,ly-dy-2*d_keyb,0]) { 
                square([lx-2*dx+2*d_keyb,dy+d_keyb]); 
            }
        }
    }
}

/*
difference () {
    inner_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy) ;
    inner_keyb2 (lx,ly,b,dx,dy,keyb_dx,keyb_dy,d_keyb);
};
*/

b2=5;
dc=8;
bc=1;

module outer_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy,b2,bc,dc) {
    difference () {
        rsquare(lx+2*keyb_dx,ly+2*keyb_dy,b2);
        inner_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy);
        translate ([dc,dc,0]) { circle (bc); }
        translate ([dc,ly+2*keyb_dy-dc,0]) { circle (bc); }
        translate ([lx+2*keyb_dx-dc,dc,0]) { circle (bc); }
        translate ([lx+2*keyb_dx-dc,ly+2*keyb_dy-dc,0]) { circle (bc); }
    }
}

module body (lx,ly,b,dx,dy,keyb_dx,keyb_dy,d_keyb,b2,bc,dc) {
    union () {
        outer_keyb (lx,ly,b,dx,dy,keyb_dx,keyb_dy,b2,bc,dc) ;
        inner_keyb2 (lx,ly,b,dx,dy,keyb_dx,keyb_dy,d_keyb);
    }
}

body (lx,ly,b,dx,dy,keyb_dx,keyb_dy,d_keyb,b2,bc,dc) ;

dy_screen=17;

module inner_screen (lx,ly,b,dx,dy,keyb_dx,keyb_dy,dy_screen) {
    translate([keyb_dx,keyb_dy,0]) {
        difference () {
            rsquare(lx,ly,b);
            square([lx,dy]); 
        }
    }
}



module screen (lx,ly,b,dx,dy,keyb_dx,keyb_dy,b2,bc,dc,dy_screen) {
    difference () { 
        rsquare(lx+2*keyb_dx,ly+2*keyb_dy,b2);
        inner_screen (lx,ly,b,dx,dy,keyb_dx,keyb_dy,dy_screen) ;
        translate ([dc,dc,0]) { circle (bc); }
        translate ([dc,ly+2*keyb_dy-dc,0]) { circle (bc); }
        translate ([lx+2*keyb_dx-dc,dc,0]) { circle (bc); }
        translate ([lx+2*keyb_dx-dc,ly+2*keyb_dy-dc,0]) { circle (bc); }
    }
}

//screen (lx,ly,b,dx,dy,keyb_dx,keyb_dy,b2,bc,dc,dy_screen);





