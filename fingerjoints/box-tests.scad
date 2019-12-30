use <box.scad>


n1 =  6;
n2 =  8; 
d1_ac = 10;
d2_ac = 10;
d1_bd = 10;
d2_bd = 10;
dy =  4;
eps = 0.01;
    
spacer_1 = 20*(n1+1);
spacer_2 = 20*(n2+1);


translate ([-2*spacer_1,0,0]) {
    translate([-2*spacer_1,0,0]) box2(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy, spacer_1, spacer_2);
    translate([spacer_1,spacer_1,0]) polygon(box_outer(n1, n2, d1_ac, d2_ac, 1.2*d1_bd, 1.2*d2_bd, dy));
    polygon(box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy));
    translate([0,-0.5*spacer_1,0]) polygon(cut_ac(n1*d1_ac, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));
    translate([10,spacer_1,0]) polygon(cut_bd(n2*(d1_bd+d2_bd)*0.6, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));    
}



lx = 120;
ly = 116;
lz = 60;
spacer_x = lx + 8;
spacer_y = ly + 8;
spacer_z = lz + 8;


box(lx, ly, lz, d1_ac,d2_ac,d1_bd,d2_bd,dy, spacer_x, spacer_y, spacer_z);
translate ([2*spacer_x,0,0]) polygon(flip_45(box_var_outer_1(ly,lz,d1_ac,d2_ac,d1_bd,d2_bd,dy))); 

translate([0,-1*(spacer_y+ly+10),0]) {
    face(box_var_inner(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));
    translate ([spacer_x,0,0]) face(box_var_inner(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));
    translate ([0,spacer_y,0]) face(box_var_outer_1(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));
    translate ([spacer_x,spacer_y,0]) face(box_var_outer_2(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));    
}

