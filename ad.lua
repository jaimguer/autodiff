-- Code based off of Jerzy Karczmarczuk's Haskell implementation,
-- found in Functional Differentation of Computer Programs
-- https://karczmarczuk.users.greyc.fr/arpap/diffalg.pdf

-- Thanks to Tim Zakian for the paper suggestion.

-- Metatable that overloads the builtin operators
mt = {
    __add = function (d1, d2)
        return {x = d1.x + d2.x, dx = d1.dx + d2.dx}
    end
    ,
    __sub = function (d1, d2)
        return {x = d1.x - d2.x, dx = d1.dx - d2.dx}
    end
    ,
    __mul = function (d1, d2)
        return {x = d1.x * d2.x ,
        dx = (d1.x * d2.dx) + (d1.dx * d2.x)}
    end
    ,
    __div = function (d1, d2)
        return {x = d1.x / d2.x,
        dx = ((d1.dx * d2.x) - (d1.x * d2.dx))
        / (d2.x * d2.x)}
    end
    ,
    __unm = function (d)
        return {x = -1 * d.x, dx = -1 * d.dx}
    end
}

function recip (d)
    return {x = 1/d.x, dx = (1 / d.x)^2 * (-1 * d.dx)}
end

-- setmetatable returns its first argument
function const (n)
    return setmetatable({x = n, dx = 0}, mt)
end

function var (n)
    return setmetatable({x = n, dx = 1.0}, mt)
end

function dlift (func, deriv)
    return function (d)
        return setmetatable({x = func(d.x), dx = d.dx * deriv(d.x)}, mt)
    end
end

function exp(x)
    return dlift(math.exp, math.exp)(x)
end

function sin(x)
    return dlift(math.sin, math.cos)(x)
end

function cos(x)
    return dlift(math.cos, function (x)
        return -1 * math.sin(x)
    end)(x)
end


function log(x)
    return dlift(math.log, function (x)
                            return 1/x end)(x)
end

function sqrt (x)
    return dlift(math.sqrt, function(x)
                                return 0.5/math.sqrt(x)
                            end)(x)
end


-- The returned values in this program aren't as nice as the way they
-- are handled in Racket. This is something I need to fix. There are
-- also a couple of functions missing from the metatable that would
-- allow for more derivates to be calculated.
-- As autodiff is a numerical method, I found I didn't need the things
-- provided by Metalua.

-- Example programs
-- > dofile 'ad.lua'
-- > function f (x)
-- >> return sin(x)
-- >> end

-- The user must specify which component (the function value or
-- derivative) they want to see.
-- Calling const on a function ensures that no derivative is calculated.

-- > print(f(const(1)).x)
-- 0.8414709848079
-- > print(f(const(1)).x)
-- 0

-- As before, calling var on an input generates both the function
-- value and its derivative.

-- > print(f(var(1)).x)
-- 0.8414709848079
-- > print(f(var(1)).dx)
-- 0.54030230586814

 
