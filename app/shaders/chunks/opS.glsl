float opS( float d1, float d2 )
{
    return max(-d1,d2);
}
vec2 opS( vec2 d1, vec2 d2 )
{
    return  -d1.x > d2.x ? d1 : d2 ;
}