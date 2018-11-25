local vec = require("vector-light")

local onescore = 0
local twoscore = 0
local stepmode = true
-- Load some default values for our rectangle.

function love.load()
    court = {}
    court.width = 800
    court.height = 600
    court.groundThickness = 10
    court.gravity = 898.2
    net = {}
    net.thickness = 20
    net.height = 150
    x, y, w, h = 20, 20, 60, 20

    color = {}
    color.r = 0.5
    color.g = 0
    color.b = 0

    player = {}
    player.xmin = 0
    player.radius = 100
    player.xmax = court.width * 0.5 - net.thickness * 0.5 - 2 * player.radius
    player.x = 100
    player.y = court.height - court.groundThickness
    player.color = color
    player.speed = 8
    player.currentVelocity = 0
    player.jumpVelocity = 500

    color2 = {}
    color2.r = 0
    color2.g = 0.5
    color2.b = 0

    player2 = {}
    player2.xmin = court.width * 0.5 + net.thickness * 0.5
    player2.radius = 100
    player2.xmax = court.width- 2 * player2.radius
    player2.x = 550
    player2.y = court.height - court.groundThickness
    player2.color = color2
    player2.speed = 8
    player2.currentVelocity = 0
    player2.jumpVelocity = 500

    ballColor = {}
    ballColor.r = .8
    ballColor.g = .8
    ballColor.b = .8

    ball = {}
    ball.y = 300
    ball.radius = 25
    ball.x = 50
    ball.xVelocity = 0
    ball.yVelocity = 0
    ball.color = ballColor
end

-- Increase the size of the rectangle every frame.
function love.update(dt)

    if stepmode == true and not love.keyboard.isDown("space") then
       return
    end
    if love.keyboard.isDown("a") then
        player.x = player.x - player.speed
        if player.x < player.xmin + player.radius then
           player.x = player.xmin + player.radius
        end
    end
    if love.keyboard.isDown("left") then
        player2.x = player2.x - player2.speed
        if player2.x < player2.xmin + player2.radius then
           player2.x = player2.xmin + player.radius
        end
    end
    if love.keyboard.isDown("d") then
        player.x = player.x + player.speed
        if player.x > player.xmax + player.radius then
           player.x = player.xmax + player.radius
        end
    end
    if love.keyboard.isDown("right") then
        player2.x = player2.x + player2.speed
        if player2.x > player2.xmax + player2.radius then
           player2.x = player2.xmax + player2.radius
        end
    end
    if love.keyboard.isDown("w") and player.currentVelocity == 0 then
        player.currentVelocity = player.jumpVelocity
    end
    if love.keyboard.isDown("up") and player2.currentVelocity == 0 then
        player2.currentVelocity = player2.jumpVelocity
    end
    if love.keyboard.isDown("escape") then
       love.event.quit()
    end
    if player.currentVelocity ~= 0 then
        player.y = player.y - player.currentVelocity * dt
        player.currentVelocity = player.currentVelocity - court.gravity * dt
    end
    if player2.currentVelocity ~= 0 then
        player2.y = player2.y - player2.currentVelocity * dt
        player2.currentVelocity = player2.currentVelocity - court.gravity * dt
    end
    if player.y > court.height - court.groundThickness then
        player.y = court.height - court.groundThickness
        player.currentVelocity = 0
    end
    if player2.y > court.height - court.groundThickness then
        player2.y = court.height - court.groundThickness
        player2.currentVelocity = 0
    end

    squareDistance1 = (player.x - ball.x) * (player.x - ball.x)
       + (player.y - ball.y) * (player.y - ball.y)
    coll1 = squareDistance1 < ((player.radius + ball.radius) *
                               (player.radius + ball.radius))

    squareDistance2 = (player2.x - ball.x) * (player2.x - ball.x)
       + (player2.y - ball.y) * (player2.y - ball.y)
    coll2 = squareDistance2 < ((player2.radius + ball.radius) *
                               (player2.radius + ball.radius))

    if coll1 then
        connx = ball.x - player.x
        conny = ball.y - player.y
        normconx, normcony = vec.normalize(connx, conny)
        perpx,perpy = vec.perpendicular(normconx, normcony)
        velx,vely = vec.mirror(-ball.xVelocity, -ball.yVelocity,
                               perpx, perpy)
        ball.xVelocity = velx
        ball.yVelocity = vely
        ball.x = player.x + normconx * (ball.radius + player.radius)
        ball.y = player.y + normcony * (ball.radius + player.radius)
        ball.color.r = 1
        ball.color.g = 1
        ball.color.b = 0
    elseif coll2 then
        connx = ball.x - player2.x
        conny = ball.y - player2.y
        normconx, normcony = vec.normalize(connx, conny)
        perpx,perpy = vec.perpendicular(normconx, normcony)
        velx,vely = vec.mirror(-ball.xVelocity, -ball.yVelocity,
                               perpx, perpy)
        ball.xVelocity = velx
        ball.yVelocity = vely
        ball.x = player2.x + normconx * (ball.radius + player2.radius)
        ball.y = player2.y + normcony * (ball.radius + player2.radius)
        ball.color.r = 1
        ball.color.g = 0
        ball.color.b = 0
    else
        ball.color.r = 1
        ball.color.g = 1
        ball.color.b = 1
    end

    ball.x = ball.x + ball.xVelocity * dt
    ball.y = ball.y - ball.yVelocity * dt
    if ball.x + ball.radius > court.width then
        ball.x = court.width - ball.radius
        ball.xVelocity = -ball.xVelocity
    end
    if ball.x < ball.radius then
        ball.x = ball.radius
        ball.xVelocity = -ball.xVelocity
    end
    if ball.y > court.height - court.groundThickness - ball.radius then
        ball.y = court.height - court.groundThickness - ball.radius
        ball.yVelocity = -ball.yVelocity

        if ball.x > court.width * 0.5 + net.thickness * 0.5 then
            onescore = onescore + 1
        end
        if ball.x < court.width * 0.5 - net.thickness * 0.5 then
            twoscore = twoscore + 1
        end
    end
    ball.yVelocity = ball.yVelocity - court.gravity * dt - 6
end

-- Draw a coloured rectangle.
function love.draw()
    love.graphics.setColor(0.4, 0.4, 0.9)
    love.graphics.rectangle("fill", 0, 0, court.width, court.height)
    love.graphics.setColor(0.5, 0.5, 0.0)
    love.graphics.rectangle("fill", 0, court.height - court.groundThickness,
                            court.width, court.groundThickness)
    love.graphics.setColor(1, 1, 1)
    love.graphics.rectangle("fill", court.width * 0.5 - net.thickness * 0.5,
                            court.height - net.height, net.thickness,
                            net.height)
    -- love.graphics.print("Hello World"..tostring(w), 400, 300)
    love.graphics.setColor(player.color.r, player.color.g, player.color.b)
    love.graphics.arc("fill", player.x, player.y, player.radius, 0,  -3.14, 8)
    love.graphics.setColor(player2.color.r, player2.color.g, player2.color.b)
    love.graphics.arc("fill", player2.x, player2.y,
                      player2.radius, 0, -3.14, 8)

    love.graphics.setColor(ball.color.r, ball.color.g, ball.color.b)
    love.graphics.circle("fill", ball.x, ball.y, ball.radius)

    love.graphics.print(tostring(onescore) .. "-" .. tostring(twoscore))

    -- debug vector
    love.graphics.line(ball.x, ball.y,
                       ball.x + ball.xVelocity/10,
                       ball.y + ball.yVelocity/10)
end
