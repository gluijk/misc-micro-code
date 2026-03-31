# Commodore 64's Impossible Mission' character GIF animation
# www.overfitting.net
# https://www.overfitting.net/2026/03/tres-micro-ejercicios-de-vibe-coding.html


library(png)

#############################################

img=readPNG("impossible_mission_ripped_sprites.png")  # 14-frame period (first and last sprite equal)
HEIGHT=nrow(img)/15  # height in pixels (33)
WIDTH=ncol(img)  # width in pixels
PERIOD=70  # period in pixels
SCALE=5  # nearest neighbour upscaling factor

# Empty canvas
canvas=array(0, dim=c(HEIGHT, 1920/SCALE, 3))
canvas[,,1]=100/255  # background colour of "impossible_mission_ripped_sprites.png"
canvas[,,2]=196/255
canvas[,,3]=173/255

nperiods=5
nframes=14
for (m in 1:nperiods) {  # generate 5 complete periods at start (we'll later crop some frames)
    offset=(m-1)*PERIOD
    for (n in 1:nframes) {  # 14 frames per period
        ini=(n-1)*HEIGHT
        imgout=canvas
        imgout[1:HEIGHT, (offset+1):(offset+WIDTH), ] = img[(ini+1):(ini+HEIGHT),,]
        imgout=imgout[, 16:(ncol(imgout)-54), ]  # keep syncronized start and end
        
        # Nearest neighbour resizing up (keep pixels appareance)
        nr <- dim(imgout)[1]
        nc <- dim(imgout)[2]
        ch <- dim(imgout)[3]
        # Generate expanded indices
        row_idx <- rep(seq_len(nr), each = SCALE)
        col_idx <- rep(seq_len(nc), each = SCALE)
        # Upscale
        img_up <- imgout[row_idx, col_idx, , drop = FALSE]
        
        # Save frame
        writePNG(img_up,  # left to right movement
                 sprintf("frame_%03d.png", (m-1)*nframes+n))
        writePNG(img_up[,ncol(img_up):1,],  # right to left movement
                 sprintf("frame_%03d.png", (m-1)*nframes+n + nperiods*nframes))       
    }
}


# Animated GIF:
# magick -delay 10 -loop 0 frame*.png impossiblemission.gif