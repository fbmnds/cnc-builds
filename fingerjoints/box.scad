
function round_e(x,e) = round(e*x)/e;

function flatten(list) = [ for (i = list, v = i) v ];

function flip_x(list) = [ for (i=list) [i[0], -1*i[1]] ];

function flip_45(list) = [ for (i=list) [i[1], i[0]] ];

function shift_x(dx, list) = [ for (i=list) [i[0]+dx, i[1]] ];

function shift_y(dy, list) = [ for (i=list) [i[0], i[1]+dy] ];

function revert(list) = [ for (i=[len(list)-1:-1:0]) list[i] ];  

function cut_bd (h, eps, list) = 
    let (l   = [ for (i = list) if (i[0] < h - eps) i ],
         fst = l[0],
         lst = l[len(l)-1])
    concat([[h, fst[1]]], l, [[h, lst[1]]]);
    
function cut_ac (h, eps, list) = 
    let (l   = [ for (i = list) if (i[1] < h - eps) [i[0],i[1]] ],
         fst = l[0],
         lst = l[len(l)-1])
    concat([[fst[0],h]], l, [[lst[0],h]]);
    
function is_elem(item, list) = 
    len([ for (i=list) if (abs(i[0] - item[0]) < 0.01 && abs(i[1] - item[1]) < 0.01) i ]) == 0 
        ? false : true;

function is_not_elem(item, list) = is_elem(item, list) ? false : true;
    
// 0 0         d1           (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
// 1 d1        d1+d2        round(i/2)*d1+floor(i/2)*d2   round(i/2)*d1+round(i/2)*d2
// 2 d1+d2     2*d1+d2      (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
// 3 2*d1+d2   2*d1+2*d2    round(i/2)*d1+floor(i/2)*d2   round(i/2)*d1+round(i/2)*d2
// 4 2*d1+2*d2 3*d1+2*d2    (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
//    __    __
// __|  |__|  ...
// 
function points_1(n, d1, d2, dy) = 
    flatten([ 
        for(i=[0:1:2*n]) i%2 == 0 ? 
            [[(i/2)*(d1+d2),              0], [(i/2+1)*d1+(i/2)*d2,        0]]  : 
            [[round(i/2)*d1+floor(i/2)*d2,dy],[round(i/2)*d1+round(i/2)*d2,dy]] ]);

//  __    __
//    |__|  |__...
// 
function points_2(n, d1, d2, dy) =
    let (pts   = points_1(n,d1,d2,dy))
    [for (i = pts) i[1] == 0 ? [i[0],dy] : [i[0],0] ];
        
function box_outer(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy) =
    let (pts_1   = points_1(n1,d1_ac,d2_ac,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_1(n2,d1_bd,d2_bd,dy),
         r_pts_2 = revert(pts_2),
         a       = pts_1,
         b       = shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_45(flip_x(pts_2))),
         c       = shift_y(n2*(d1_bd+d2_bd)+d1_bd,flip_x(r_pts_1)),
         d       = flip_45(r_pts_2))    
   flatten([a, b, c, d]); 

function box_outer_2(n1, n2, d1_ac, d2_ac, dy) =
    let (pts_1   = points_1(n1,d1_ac,d2_ac,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_2(n2,d1_ac,d2_ac,dy),
         r_pts_2 = revert(pts_2))    
   flatten([pts_1, 
     shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_45(flip_x(pts_2))), 
     shift_y(n2*(d1_ac+d2_ac)+d1_ac,(flip_x(r_pts_1))), 
     flip_45(r_pts_2)
     ]); 

function box_outer_3(n1, n2, d1_ac, d2_ac, dy) =
    let (pts_1   = points_1(n1,d1_ac,d2_ac,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_2(n2,d1_ac,d2_ac,dy),
         r_pts_2 = revert(pts_2),
         p = [[0,0], 
             [n1*(d1_ac+d2_ac)+d1_ac,0], 
             [n1*(d1_ac+d2_ac)+d1_ac,n2*(d1_ac+d2_ac)+d1_ac], 
             [0,n2*(d1_ac+d2_ac)+d1_ac], 
             ],
         l = flatten([
             [[dy,0]], 
             pts_1, 
             [[n1*(d1_ac+d2_ac)+d1_ac-dy,0]],
             shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_45(flip_x(pts_2))), 
             [[n1*(d1_ac+d2_ac)+d1_ac-dy,n2*(d1_ac+d2_ac)+d1_ac]],
             shift_y(n2*(d1_ac+d2_ac)+d1_ac,(flip_x(r_pts_1))), 
             [[dy,n2*(d1_ac+d2_ac)+d1_ac]],
             flip_45(r_pts_2),
             [[dy,0]]
             ]))
     [ for (i = l) if (is_not_elem(i,p)) i ];         

function box_inner(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy) =
    let (pts_1   = points_1(n1,d1_ac,d2_ac,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_1(n2,d1_bd,d2_bd,dy),
         r_pts_2 = revert(pts_2),
         p = [[dy, -1*dy],
              [n1*(d1_ac+d2_ac)+d1_ac,0],  
              [n1*(d1_ac+d2_ac)+d1_ac,n2*(d1_bd+d2_bd)+d1_bd-2*dy],
              [n1*(d1_ac+d2_ac)+d1_ac-dy,n2*(d1_bd+d2_bd)+d1_bd-dy],
              [dy,n2*(d1_bd+d2_bd)+d1_bd-dy],
              [0,n2*(d1_bd+d2_bd)+d1_bd-2*dy],
              ], 
         a = flip_x(pts_1),
         b = shift_y(-1*dy,shift_x(-1*dy,shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_45(pts_2)))),
         c = shift_y(n2*(d1_bd+d2_bd)+d1_bd-2*dy,r_pts_1),
         d = shift_y(-1*dy,shift_x(dy,flip_45(flip_x(r_pts_2)))),
         l = flatten([
                [[dy,0]],
                a, 
                [[n1*(d1_ac+d2_ac)+d1_ac-dy,0]],
                b,
                [[n1*(d1_ac+d2_ac)+d1_ac-dy,n2*(d1_bd+d2_bd)+d1_bd-2*dy]],
                c, 
                [[dy,n2*(d1_bd+d2_bd)+d1_bd-2*dy]],
                d,
                [[dy,0]]
                ]))
    shift_y(dy,[ for (i = l) if (is_not_elem(i,p)) i ]);

//translate([0,10,0]) polygon(box_inner_2(n, d1_ac, d2_ac, dy));

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

module box2 (n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy, spacer_1, spacer_2) {        
    polygon(box_inner(n1, n1, d1_ac, d2_ac, d1_ac, d2_ac, dy));
    translate([spacer_1,0,0]) polygon(box_inner(n1, n1, d1_ac, d2_ac, d1_ac, d2_ac, dy));
    translate([spacer_1,spacer_1,0]) polygon(box_outer(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy));
    translate([0,spacer_1,0]) polygon(box_outer_2(n1, n2, d1_ac, d2_ac, dy));    
    translate([spacer_1,spacer_1+spacer_2,0]) polygon(box_outer(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy));   
    translate([0,spacer_1+spacer_2,0]) polygon(box_outer_2(n1, n2, d1_ac, d2_ac, dy));  
}

translate ([-2*spacer_1,0,0]) {
    translate([-2*spacer_1,0,0]) box2(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy, spacer_1, spacer_2);
    translate([spacer_1,spacer_1,0]) polygon(box_outer(n1, n2, d1_ac, d2_ac, 1.2*d1_bd, 1.2*d2_bd, dy));
    polygon(box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy));
    translate([0,-0.5*spacer_1,0]) polygon(cut_ac(n1*d1_ac, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));
    translate([10,spacer_1,0]) polygon(cut_bd(n2*(d1_bd+d2_bd)*0.6, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));    
}

//       __         __
// _____|  |__...__|  |_____
// 
function points_var_1(l, d1, d2, dy) = 
    let (m    = floor(l/(d1+d2)),
         tail = l - m*(d1+d2), 
         n    = (tail - d1) > 0 ? m : m-1,
         off  = (tail - d1) > 0 ? round_e((tail-d1)/2,10) : round_e((tail+d1)/2,10))
    concat([[0,0],[off,0]], shift_x(off, points_1(n,d1,d2,dy)),[[l,0]]);
        
    

//  _____    ..    _____
//       |__|  |__|
// 
function shift_flip(dy, pts) = [ for (i = pts) i[1] == 0 ? [i[0],dy] : [i[0],0] ];


// size lx * ly
//     
function box_var_inner(lx, ly, d1_1, d1_2, d2_1, d2_2, dy) =
    let (l1   = lx - 2*dy,
         l2   = ly - 2*dy,
         p_l1 = points_var_1(l1, d1_1, d1_2, dy),
         p_a  = shift_flip(dy, p_l1),
         p_c  = revert(p_l1),
         p_l2 = points_var_1(l2, d2_1, d2_2, dy),
         p_b  = flip_45(shift_flip(dy, p_l2)),
         p_d  = flip_45(revert(p_l2)))
    flatten([shift_x(dy,    p_a),
                            shift_y(dy,    p_b),
             shift_x(dy,    shift_y(l2+dy, p_c)),
             shift_x(l1+dy, shift_y(dy, p_d))]);


// size lx * ly
// 
function box_var_outer_1(lx, ly, d1_1, d1_2, d2_1, d2_2, dy) =
    let (l1   = lx - 2*dy,
         l2   = ly - 2*dy,
         p_a  = points_var_1(l1, d1_1, d1_2, dy),
         p_c  = revert(shift_flip(dy,p_a)),
         p_l2 = concat([[0,0]], shift_x(dy,points_var_1(l2, d2_1, d2_2, dy)), [[ly,0]]),
         p_b  = flip_45(flip_x(p_l2)),
         p_d  = flip_45(revert(p_l2)))
    flatten([p_a,
             shift_x(l1+dy, p_b),
             shift_y(l2+dy, p_c),
             p_d]);

lx = 120;
ly = 116;
spacer_x = lx + 8;
spacer_y = ly + 8;

difference() {
    polygon([[0,0],[lx,0],[lx,ly],[0,ly],[0,0]]);
    polygon(box_var_inner(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));
}

translate ([spacer_x,0,0]) polygon(box_var_outer_1(lx,ly,d1_ac,d2_ac,d1_bd,d2_bd,dy));