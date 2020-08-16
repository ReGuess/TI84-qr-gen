
# With slight modification from
# https://en.wikiversity.org/wiki/Reed%E2%80%93Solomon_codes_for_coders


def gf_mul(x, y, prim=0x11d, field_charac_full=256):
    r = 0
    while y:
        if y & 1:
            r ^= x
        y = y >> 1
        x = x << 1
        if prim > 0 and x & field_charac_full:
            x ^= prim
    return r



def gf_pow(x, power):
    a = 1
    for b in range(power):
        a = gf_mul(x, a)
    return a


def gf_poly_mul(p, q):
    r = [0] * (len(p)+len(q)-1)
    for j in range(len(q)):
        for i in range(len(p)):
            r[i+j] ^= gf_mul(p[i], q[j])
    return r




def rs_generator_poly(nsym):
    g = [1]
    a = 1
    for i in range(nsym):
        g = gf_poly_mul(g, [1, gf_pow(2, i)])
    return g



def rs_encode_msg(msg_in, nsym):
    if (len(msg_in) + nsym) > 255:
        raise ValueError("Message is too long (%i when max is 255)" % (len(msg_in)+nsym))
    gen = rs_generator_poly(nsym)
    #msg_out = [0] * (len(msg_in) + len(gen)-1)
    #msg_out[:len(msg_in)] = msg_in
    msg_out = list(msg_in + [0]*nsym)
    print(msg_out)
    for i in range(len(msg_in)):
        coef = msg_out[i]
        if coef:
            for j in range(1, len(gen)):
                msg_out[i+j] ^= gf_mul(gen[j], coef)
        print(msg_out)
    msg_out[:len(msg_in)] = msg_in
    return msg_out


# HELLO WORLD
my_msg = [32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236, 17, 236, 17]
rs_encode_msg(my_msg, 10)
