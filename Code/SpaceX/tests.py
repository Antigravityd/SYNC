from parse_file import parseFile
from structures import PixelDataToAToT

with open("hex.txt", 'r', encoding="ascii") as data:
    result = parseFile(data)
    total = []
    count = 0
    currentFrame = result[0].frame_id
    # for j in result:

    #     if j.mode == 0:
    #         res = [k.x + 256 * (k.y-1) for k in j.pixel_data]
    #         if j.frame_id != currentFrame:
    #             print(res)

    #             currentFrame = j.frame_id
    #             break
    #         else:
    #             total.extend(res)
    for i in result[0].pixel_data:
        print(i.x, i.y, i.tot, i.toa, i.ftoa)
# print(set([x for x in total if total.count(x) > 1]))
