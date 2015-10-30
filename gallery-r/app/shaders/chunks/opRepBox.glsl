float opRepBox( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return udBox( q  ,vec3( r ));
}