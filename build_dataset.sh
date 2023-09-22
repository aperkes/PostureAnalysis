## First , read off the list of files

fishtrack_path='/home/ammon/Documents/Scripts/AnalyzePosture/video_processing/'
working_dir=$fishtrack_path"working_dir/"

#vid_file=$fishtrack_path'/test_vid_list.txt'
vid_file=$fishtrack_path'bv2-2019_vid_list.txt'
vid_dir='/data/postures/birdview2-2019/'
merge_file=$fishtrack_path'bv2-19_clip_list.txt'

vid_list=$(cat $vid_file)

echo $vid_list

for f in $vid_list; do
    echo $f
    #rclone copy aperkes:pivideos/$f $working_dir -P
    base_f=$(basename $f)

    #out_path=${video_path%.mp4}'.clip.mp4'
    out_path=$working_dir${base_f%.mp4}'.clip.mp4'
    video_path=$vid_dir$base_f
    #ffmpeg -i $video_path -vf "select='between(t,220,221) + between(t,320,321) + between(t,420,421) + between(t,520,521) + between(t,620,621) + between(t,720,721) + between(t,820,821) + between(t,920,921) + between(t,1020,1021) + between(t,1120,1121) + between(t,1220,1221) + between(t,1320,1321) + between(t,1420,1421) + between(t,1520,1521) + between(t,1620,1621)', setpts=N/FRAME_RATE/TB" $out_path -n
    echo $f 
    ffmpeg -i $f -vf "select='between(t,1,1.2) + between(t,2,2.2)', setpts=N/FRAME_RATE/TB" $out_path -n

    echo file $out_path >> $merge_file 
done

outfile=${vid_file%.txt}'.mp4'

ffmpeg -f concat -safe 0 -i "$merge_file" $outfile

