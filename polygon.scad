
function flatten(list) = [ for (i = list, v = i) v ];

function flip_x(list) = [ for (i=list) [i[0], -1*i[1]] ];

function flip_90(list) = [ for (i=list) [i[1], i[0]] ];

function shift_x(dx, list) = [ for (i=list) [i[0]+dx, i[1]] ];

function shift_y(dy, list) = [ for (i=list) [i[0], i[1]+dy] ];

function revert(list) = [ for (i=[len(list)-1:-1:0]) list[i] ];  

function is_elem(item, list) = 
    len([ for (i=list) if (abs(i[0] - item[0]) < 0.01 && abs(i[1] - item[1]) < 0.01) i ]) == 0 
        ? false : true;

function is_not_elem(item, list) = is_elem(item, list) ? false : true;
    
// 0 0         d1           (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
// 1 d1        d1+d2        round(i/2)*d1+floor(i/2)*d2   round(i/2)*d1+round(i/2)*d2
// 2 d1+d2     2*d1+d2      (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
// 3 2*d1+d2   2*d1+2*d2    round(i/2)*d1+floor(i/2)*d2   round(i/2)*d1+round(i/2)*d2
// 4 2*d1+2*d2 3*d1+2*d2    (i/2)*(d1+d2)                 (i/2+1)*d1+(i/2)*d2
//
function points_1(n, d1, d2, dy) = 
    flatten([ 
        for(i=[0:1:2*n]) i%2 == 0 ? 
            [[(i/2)*(d1+d2),              0], [(i/2+1)*d1+(i/2)*d2,        0]]  : 
            [[round(i/2)*d1+floor(i/2)*d2,dy],[round(i/2)*d1+round(i/2)*d2,dy]] ]);

function points_2(n, d1, d2, dy) =
    let (pts   = points_1(n,d1,d2,dy))
    [for (i = pts) i[1] == 0 ? [i[0],dy] : [i[0],0] ];
        
function sq_outer(n, d1, d2, dy) =
    let (pts   = points_1(n,d1,d2,dy),
         r_pts = revert(pts))    
   flatten([pts, 
     shift_x(n*(d1+d2)+d1,flip_90(flip_x(pts))), 
     shift_y(n*(d1+d2)+d1,(flip_x(r_pts))), 
     flip_90(r_pts)
     ]); 

function sq_outer_2(n, d1, d2, dy) =
    let (pts_1   = points_1(n,d1,d2,dy),
         r_pts_1 = revert(pts_1),
         pts_2   = points_2(n,d1,d2,dy),
         r_pts_2 = revert(pts_2),
         p = [[0,0], 
             [n*(d1+d2)+d1,0], 
             [n*(d1+d2)+d1,n*(d1+d2)+d1], 
             [0,n*(d1+d2)+d1], 
             ],
         l = flatten([
             [[dy,0]], 
             pts_1, 
             [[n*(d1+d2)+d1-dy,0]],
             shift_x(n*(d1+d2)+d1,flip_90(flip_x(pts_2))), 
             [[n*(d1+d2)+d1-dy,n*(d1+d2)+d1]],
             shift_y(n*(d1+d2)+d1,(flip_x(r_pts_1))), 
             [[dy,n*(d1+d2)+d1]],
             flip_90(r_pts_2),
             [[dy,0]]
             ]))
     [ for (i = l) if (is_not_elem(i,p)) i ];         
        
        
function sq_inner_2(n, d1, d2, dy) =
    let (pts   = points_1(n,d1,d2,dy),
         r_pts = revert(pts))    
   flatten([flip_x(pts), 
     shift_x(n*(d1+d2)+d1,flip_90(pts)), 
     shift_y(n*(d1+d2)+d1,(pts)), 
     flip_90(flip_x(r_pts))
     ]);

function sq_inner(n, d1, d2, dy) =
    let (pts   = points_1(n,d1,d2,dy),
         r_pts = revert(pts),
         p     = [ 
                  [dy, -1*dy],
                  [n*(d1+d2)+d1,0],  
                  [n*(d1+d2)+d1,n*(d1+d2)+d1-2*dy],
                  [n*(d1+d2)+d1-dy,n*(d1+d2)+d1-dy],
                  [dy,n*(d1+d2)+d1-dy],
                  [0,n*(d1+d2)+d1-2*dy]
                  ], 
         l     = flatten([[[dy,0]],
                         flip_x(pts), 
                         [[n*(d1+d2)+d1-dy,0]],
                         shift_y(-1*dy,shift_x(-1*dy,shift_x(n*(d1+d2)+d1,flip_90(pts)))),
                         [[n*(d1+d2)+d1-dy,n*(d1+d2)+d1-2*dy]],
                         shift_y(n*(d1+d2)+d1-2*dy,r_pts), 
                         [[dy,n*(d1+d2)+d1-2*dy]],
                         shift_y(-1*dy,shift_x(dy,flip_90(flip_x(r_pts)))),
                         [[dy,0]]
                         ]))
    shift_y(dy,[ for (i = l) if (is_not_elem(i,p)) i ]);



polygon(sq_inner(3, 1, 1, 0.4));

//translate([0,10,0]) polygon(sq_inner_2(3, 1, 1, 0.4));

translate([8,0,0]) polygon(sq_outer(3, 1, 1, 0.4));
    
translate([0,8,0]) polygon(sq_outer_2(3, 1, 1, 0.4));    

translate([8,8,0]) polygon(sq_inner(3, 1, 1, 0.4));

translate([0,16,0]) polygon(sq_outer(3, 1, 1, 0.4));
    
translate([8, 16,0]) polygon(sq_outer_2(3, 1, 1, 0.4));  
