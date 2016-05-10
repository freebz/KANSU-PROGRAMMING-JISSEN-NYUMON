/* coord.c
   $ gcc -o coord coord.c -W -Wall -Wextra -lm -ansi -pedantic
   $ ./coord
   (-0.000000,-0.707107)
   (-0.707107,0.000000)
   (0.000000,0.707107)
   (0.707107,-0.000000)
*/
#include <stdio.h>
#include <math.h>

/* 좌표의 타입 */
typedef struct {
  double x;
  double y;
} coord_t;

/* 좌표 변환 설정 */
typedef struct {
  coord_t rotAt; /* 회전 중심 좌표 */
  double  theta; /* 회전량[라디안] */
  double  ofs_x; /* X축 방향 병행 이동량 */
  double  ofs_y; /* Y출 방향 병행 이동량 */
} config_t;

typedef coord_t (*converter_t)(coord_t);

/* 병행 이동의 프리미티브 */
coord_t trans(double dx, double dy, coord_t coord) {
  coord_t result = coord;
  result.x += dx;
  result.y += dy;
  return result;
}

/* 원점 중심 회전의 프리미티브 */
coord_t rotate(double theta, coord_t coord) {
  coord_t result = {0, 0};
  result.x = cos(theta) * coord.x - sin(theta) * coord.y;
  result.y = sin(theta) * coord.x + cos(theta) * coord.y;
  return result;
}

/* 설정을 베이스로 한 병행 이동 */
coord_t trans_by_config(config_t config, coord_t coord) {
  return trans(config.ofs_x, config.ofs_y, coord);
}

/* 설정을 베이스로 한 회전 */
coord_t rotate_by_config(config_t config, coord_t coord) {
  coord_t pre_trans  = trans(-config.rotAt.x, -config.rotAt.y, coord);
  coord_t rotated    = rotate(config.theta, pre_trans);
  coord_t post_trans = trans(config.rotAt.x, config.rotAt.y, rotated);
  return post_trans;
}

/* 설정을 베이스로 한 좌표 변환 */
coord_t convert_by_config(config_t config, coord_t coord) {
  return trans_by_config(config, rotate_by_config(config, coord));
}

/* 좌표 모두에 동일한 변환을 적용 */
void map_to_coords(converter_t conv, size_t n, coord_t* in_coord, coord_t* out_coord) {
  unsigned int i = 0;
  for (i = 0; i < n; i++) out_coord[i] = conv(in_coord[i]);
}

int main() {
  /* (0.5, 0.5)을 중심으로 반시계 방향으로 45도 회전시키고, (-0.5, -0.5) 병행 이동시크는 설정 */
  config_t config = { {0.5, 0.5}, 3.141592653589793 / 4, -0.5, -0.5 };
  /* 변환 전의 좌표, 예를 들면 네 개의 점으로부터 만들어지는 정사각형 */
  coord_t unit_rect[] = { {0, 0}, {0, 1}, {1, 1}, {1, 0} };
  /* 변환 후의 좌표 */
  coord_t converted_rect[] = { {0, 0}, {0, 0}, {0, 0}, {0, 0} };

  /*
    map_to_coords를 사용하고 싶지만,
    convert_by_config와 간단히 조합시킬 수 없어
    어쩔 수 없이 루프
  */
  { unsigned int i = 0;
    for (i = 0; i < sizeof(unit_rect)/sizeof(unit_rect[0]); i++)
      converted_rect[i] = convert_by_config(config, unit_rect[i]);
  }

  { unsigned int i = 0;
    for (i = 0; i < sizeof(converted_rect)/sizeof(converted_rect[0]); i++)
      printf("(%.6f,%.6f)\n", converted_rect[i].x, converted_rect[i].y);
  }
  return 0;
}
