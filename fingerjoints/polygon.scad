
function flatten(list) = [ for (i = list, v = i) v ];

function flip_x(list) = [ for (i=list) [i[0], -1*i[1]] ];

function flip_90(list) = [ for (i=list) [i[1], i[0]] ];

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
// __|  |__|  |__...
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
         b       = shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_90(flip_x(pts_2))),
         c       = shift_y(n2*(d1_bd+d2_bd)+d1_bd,flip_x(r_pts_1)),
         d       = flip_90(r_pts_2))    
   flatten([a, b, c, d]); 

function box_outer_2(n1, n2, d1_ac, d2_ac, dy) =
    let (pts_1   = points_1(n1,d1_ac,d2_ac,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_2(n2,d1_ac,d2_ac,dy),
         r_pts_2 = revert(pts_2))    
   flatten([pts_1, 
     shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_90(flip_x(pts_2))), 
     shift_y(n2*(d1_ac+d2_ac)+d1_ac,(flip_x(r_pts_1))), 
     flip_90(r_pts_2)
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
             shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_90(flip_x(pts_2))), 
             [[n1*(d1_ac+d2_ac)+d1_ac-dy,n2*(d1_ac+d2_ac)+d1_ac]],
             shift_y(n2*(d1_ac+d2_ac)+d1_ac,(flip_x(r_pts_1))), 
             [[dy,n2*(d1_ac+d2_ac)+d1_ac]],
             flip_90(r_pts_2),
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
         b = shift_y(-1*dy,shift_x(-1*dy,shift_x(n1*(d1_ac+d2_ac)+d1_ac,flip_90(pts_2)))),
         c = shift_y(n2*(d1_bd+d2_bd)+d1_bd-2*dy,r_pts_1),
         d = shift_y(-1*dy,shift_x(dy,flip_90(flip_x(r_pts_2)))),
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

translate([-2*spacer_1,0,0]) box2(n1, n2, d1_ac, d2_ac, d1_bd, d2_bd, dy, spacer_1, spacer_2);

translate([spacer_1,spacer_1,0]) polygon(box_outer(n1, n2, d1_ac, d2_ac, 1.2*d1_bd, 1.2*d2_bd, dy));

polygon(box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy));

translate([0,-0.5*spacer_1,0]) polygon(cut_ac(n1*d1_ac, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));

translate([0,spacer_1,0]) polygon(cut_bd(n2*(d1_bd+d2_bd)*0.6, eps, box_inner(n2, n1, d1_bd, d2_bd, d1_ac, d2_ac, dy)));