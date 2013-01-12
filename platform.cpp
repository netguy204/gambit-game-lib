#include "platform.h"
#include "config.h"

#include <math.h>
#include <stdarg.h>

OBJECT_IMPL(CPlatformer, Component);
OBJECT_PROPERTY(CPlatformer, w);
OBJECT_PROPERTY(CPlatformer, h);
OBJECT_PROPERTY(CPlatformer, density);
OBJECT_PROPERTY(CPlatformer, friction);
OBJECT_PROPERTY(CPlatformer, parent);

CPlatformer::CPlatformer(void* go)
  : Component((GO*)go, PRIORITY_THINK), w(10), h(10),
    density(1), friction(0.2), top_fixture(NULL),
    bottom_fixture(NULL), box_fixture(NULL), parent(NULL) {
  this->go->body->SetType(b2_dynamicBody);
}

CPlatformer::~CPlatformer() {
  go->body->DestroyFixture(bottom_fixture);
  if(top_fixture) {
    go->body->DestroyFixture(top_fixture);
    go->body->DestroyFixture(box_fixture);
  }
  go->body->SetType(b2_staticBody);
}

void CPlatformer::init() {
  // build a pill shaped collider so that we don't get stuck on seams

  // top circle
  b2FixtureDef fixtureDef;
  fixtureDef.friction = friction;
  fixtureDef.density = density;

  float radius = w / 2;
  float box_height = fmax(0, h - 2 * radius);

  b2CircleShape bottom;
  bottom.m_p.Set(0, -(box_height / 2) / BSCALE);
  bottom.m_radius = radius / BSCALE;
  fixtureDef.shape = &bottom;
  bottom_fixture = go->body->CreateFixture(&fixtureDef);
  bottom_fixture->SetUserData(this);

  // bottom circle
  if(box_height > 0) {
    b2CircleShape top;
    top.m_p.Set(0, (box_height / 2) / BSCALE);
    top.m_radius = radius / BSCALE;
    fixtureDef.shape = &top;
    top_fixture = go->body->CreateFixture(&fixtureDef);
    top_fixture->SetUserData(this);

    // box to fill out the area between the circles
    b2PolygonShape box;
    box.SetAsBox((w/2)/BSCALE, (box_height/2)/BSCALE);
    fixtureDef.shape = &box;

    box_fixture = go->body->CreateFixture(&fixtureDef);
    box_fixture->SetUserData(this);
  }
}

void CPlatformer::update(float dt) {
  // try to find a contact with our fixture
  b2ContactEdge* node = go->body->GetContactList();
  GO* old_parent = parent;
  Vector_ ourpos;
  go->pos(&ourpos);

  parent = NULL;

  while(node) {
    if(node->contact->IsTouching() &&
       (node->contact->GetFixtureA() == bottom_fixture ||
        node->contact->GetFixtureB() == bottom_fixture)) {

      // need to ensure normal is up
      GO* other = (GO*)node->other->GetUserData();
      b2WorldManifold manifold;
      node->contact->GetWorldManifold(&manifold);

      // normal goes from A to B so we need to be careful
      if((node->contact->GetFixtureA() == bottom_fixture && manifold.normal.y < 0)
         || manifold.normal.y > 0) {

        // normal is pointing the right way but the contact must be
        // below us
        if((manifold.points[0].y*BSCALE) < ourpos.y) {
          parent = other;
          break;
        }
      }
    }
    node = node->next;
  }

  if(parent != old_parent) {
    go->send_message(go->create_message(MESSAGE_PARENT_CHANGE, NULL, 0));
  }
}
